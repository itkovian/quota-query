{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad          (mzero)
import           Data.Aeson
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Data.Text              (Text, concat, isPrefixOf, pack)
import           Data.Time.Calendar     (Day (..))
import           Data.Time.Clock        (UTCTime (..), secondsToDiffTime, getCurrentTime, addUTCTime)
import qualified Data.Vector            as V
import           Database.V5.Bloodhound
import           Database.V5.Bloodhound.Types
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (defaultManagerSettings, responseBody)
import           System.Console.CmdArgs
import           Text.Printf

import           Query
import           Quota
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{- ElasticSearch data types -}
data SourceEntry = SourceEntry
  { quota :: Quota
  } deriving (Eq, Generic, Show)

instance ToJSON SourceEntry where
    toJSON = genericToJSON defaultOptions
instance FromJSON SourceEntry where
    parseJSON = withObject "source" $ \o -> do
        quota <- o .: "quota"
        return SourceEntry { quota = quota }
-------------------------------------------------------------------------------
{- Program options -}

data Options =
    User
      { vsc_id :: String
      , storage_fileset :: String
      , storage_filesystem :: String
      }
  deriving (Data,Typeable,Show,Eq)

user = User
  { vsc_id = def &= help "VSC ID of the requested user"
  , storage_fileset = def &= help "Fileset name (defaults to the personal storage fileset)"
  , storage_filesystem = def &= help "Filesystem (GPFS device) for which we seek usage information"
  } &= help "Usage information of users"

------------------------------------------------------------------------------y
{-

# modes

- show_quota
    - for a user: shows all the (latest) usage


-}
main :: IO ()
main = do
    opts <- cmdArgs user

    -- determine which query we need to run that matches the provided mode and options
    query <- showQuotaQueryUSR "vsc40075"

    qs <- runBH' $ do
        -- do a search
        let search = mkSearch (Just query) Nothing
        response <- searchByIndex testIndex search -- :: BH IO (Either EsError (SearchResult [Quota]))

        let result = decode (responseBody response) :: Maybe (SearchResult SourceEntry)

        liftIO $ case result of
            Just r -> return $ Just . map quota . catMaybes . map hitSource . hits . searchHits $ r
            Nothing -> return Nothing

    case qs of
        Nothing -> print "No results found"
        Just qs -> putStrLn $ unlines $ map show qs

  where
    testServer = (Server "http://localhost:9200")
    runBH' = withBH defaultManagerSettings testServer
    testIndex = IndexName "longterm-2017"
    testMapping = MappingName "gpfsbeat"
    indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
