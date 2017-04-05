{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Quota (Quota(..)) where

import           Control.Monad          (mzero)
import           Data.Aeson
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Data.Text              (Text, concat, isPrefixOf, pack)
import           GHC.Generics           (Generic)
import           Text.Printf

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

-- Representatin of quota/usage information in the ES indices
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
