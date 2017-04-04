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
import           Data.Text              (Text)
import           Data.Time.Calendar     (Day (..))
import           Data.Time.Clock        (UTCTime (..), secondsToDiffTime)
import qualified Data.Vector            as V
import           Database.V5.Bloodhound
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (defaultManagerSettings, responseBody)
import           System.Console.CmdArgs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{- ElasticSearch data types -}
data SourceEntry = SourceEntry
  { quota :: Quota
  } deriving (Eq, Generic, Show)

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
  parseJSON (Object o) = Quota
      <$> o .: "filesystem"
      <*> o .: "fileset"
      <*> o .: "kind"
      <*> o .: "entity"
      <*> o .: "block_usage"
      <*> o .: "block_soft"
      <*> o .: "block_hard"
      <*> o .: "block_doubt"
      <*> o .: "block_expired"
      <*> o .: "files_usage"
      <*> o .: "files_soft"
      <*> o .: "files_hard"
      <*> o .: "files_doubt"
      <*> o .: "files_expired"
  parseJSON _ = mzero

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

constructQuery :: Options -> Query
constructQuery _ = QueryBoolQuery BoolQuery
    { boolQueryMustMatch =
        [ TermQuery (Term "quota.filesystem" "scratchdelcatty") Nothing
        , TermQuery (Term "quota.kind" "FILESET") Nothing
        , TermQuery (Term "quota.fileset" "gvo00003") Nothing
        ]
    , boolQueryFilter = []
    , boolQueryMustNotMatch = []
    , boolQueryShouldMatch = []
    , boolQueryMinimumShouldMatch = Nothing
    , boolQueryBoost = Nothing
    , boolQueryDisableCoord = Nothing
    }

------------------------------------------------------------------------------y
main :: IO ()
main = do
  opts <- cmdArgs user

  let query = constructQuery opts

  runBH' $ do
    -- do a search
    let query = constructQuery opts
    let search = mkSearch (Just query) Nothing
    response <- searchByIndex testIndex search -- :: BH IO (Either EsError (SearchResult [Quota]))

    let result = decode (responseBody response) :: Maybe (SearchResult SourceEntry)

    liftIO $ putStrLn $ show result

    return ()
  return ()
  where
    testServer = (Server "http://localhost:9200")
    runBH' = withBH defaultManagerSettings testServer
    testIndex = IndexName "longterm-2017"
    testMapping = MappingName "gpfsbeat"
    indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
