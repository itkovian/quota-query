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
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
{- HPCUgent-specific mappings -}
hpcugentFilesetPrefixes = M.fromList
    [ ("personal", "vsc")
    , ("vo", "gvo")
    ]

hpcugentDeviceMappings :: M.Map Text Text
hpcugentDeviceMappings = M.fromList
    [ ("vulpixhome", "VSC_HOME")                    -- device/filesystem holding home
    , ("vulpixdata", "VSC_DATA")                    -- device/filesystem holding long term data
    , ("scratchdelcatty", "VSC_SCRATCH_DELCATTY")   -- device/filesystem holding shared scratch on delcatty
    , ("scratchphanpy", "VSC_SCRATCH_PHANPY")       -- device/filesystem holding shared scratch on phanpy
    ]

determineUnit :: Integer -> (Double, Text)
determineUnit v = d (fromInteger v) ["KiB", "MiB", "GiB", "TiB"]
  where d :: Double -> [Text] -> (Double, Text)
        d v [] = (v, "TiB")
        d v (u:us)
            | v < 1024 = (v, u)
            | otherwise = d (v / 1024.0) us


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
  } deriving (Eq, Generic)

instance Show Quota where
    show q =
        let voSuffix = if isPrefixOf "gvo" (fileset q) then Data.Text.concat ["_VO (", fileset q, ")"] else ""
            deviceName = Data.Text.concat [fromJust $ M.lookup (filesystem q) hpcugentDeviceMappings, voSuffix]
            (usage, usageUnit) = determineUnit (blockUsage q)
            (softLimit, softLimitUnit) = determineUnit (blockSoft q)
            (hardLimit, hardLimitUnit) = determineUnit (blockHard q)
        in printf "%s: used %.2f %s (%d%%) quota %.2f %s (%.2f %s hard limit)" deviceName usage usageUnit (if (blockUsage q) == 0 then 0 else floor $ 100 * (fromInteger $ blockUsage q) / (fromInteger $ blockSoft q) :: Integer) softLimit softLimitUnit hardLimit hardLimitUnit

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
        [ --TermQuery (Term "quota.filesystem" "vulpixdata") Nothing
         TermQuery (Term "quota.kind" "FILESET") Nothing
        --, TermQuery (Term "quota.fileset" "gvo00003") Nothing
        ]
    , boolQueryFilter = []
    , boolQueryMustNotMatch = []
    , boolQueryShouldMatch = []
    , boolQueryMinimumShouldMatch = Nothing
    , boolQueryBoost = Nothing
    , boolQueryDisableCoord = Nothing
    }

showQuotaQueryUSR :: String -> IO Query
showQuotaQueryUSR v = do
    now <- getCurrentTime
    let vscID = pack v
        timeLimit = RangeDateGte (GreaterThanEqD $ addUTCTime (negate 3600) now)
    return $ QueryBoolQuery $ BoolQuery
        { boolQueryMustMatch =
            [ TermQuery (Term "quota.entity" vscID) Nothing
            , QueryRangeQuery $ mkRangeQuery (FieldName "@timestamp") timeLimit
            ]
        , boolQueryFilter = []
        , boolQueryMustNotMatch = [ QueryMatchQuery $ mkMatchQuery (FieldName "quota.kind") (QueryString "GRP") ]
        , boolQueryShouldMatch = []
        , boolQueryMinimumShouldMatch = Nothing
        , boolQueryBoost = Nothing
        , boolQueryDisableCoord = Nothing
        }
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
