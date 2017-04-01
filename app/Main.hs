{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON (..), defaultOptions,
                                         genericParseJSON, genericToJSON,
                                         object, (.=))
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Text              (Text)
import           Data.Time.Calendar     (Day (..))
import           Data.Time.Clock        (UTCTime (..), secondsToDiffTime)
import qualified Data.Vector            as V
import           Database.V1.Bloodhound
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (defaultManagerSettings)
-------------------------------------------------------------------------------


data Quota = Quota
  { filesystem :: Text
  , fileset    :: Text
  , kind       :: Text
  , entity     :: Text
  , blockUsage :: Integer
  , blockSoft  :: Integer
  , blockHard  :: Integer
  , blockDoubt :: Integer
  , blockGrace :: Text
  , filesUsage :: Integer
  , filesSoft  :: Integer
  , filesHard  :: Integer
  , filesDoubt :: Integer
  , filesGrace :: Text
  } deriving (Eq, Generic, Show)

-------------------------------------------------------------------------------
instance ToJSON Quota where
  toJSON = genericToJSON defaultOptions
instance FromJSON Quota where
  parseJSON = genericParseJSON defaultOptions

-------------------------------------------------------------------------------
main :: IO ()
main = runBH' $ do
  -- do a search
  let boost = Nothing
  let query = TermQuery (Term "quota.filesystem" "scratchdelcatty") boost
  let search = mkSearch (Just query) boost
  res <- searchByType testIndex testMapping search

  liftIO $ putStrLn $ show res

  return ()
  where
    testServer = (Server "http://localhost:9200")
    runBH' = withBH defaultManagerSettings testServer
    testIndex = IndexName "gpfsbeat-*"
    testMapping = MappingName "gpfsbeat"
    indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
