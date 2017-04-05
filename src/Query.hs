{-# LANGUAGE OverloadedStrings #-}

module Query
    ( constructQuery
    , showQuotaQueryUSR
    ) where

import           Data.Text              (Text, concat, isPrefixOf, pack)
import           Data.Time.Clock        (UTCTime (..), secondsToDiffTime, getCurrentTime, addUTCTime)
import           Database.V5.Bloodhound
import           Database.V5.Bloodhound.Types
import           System.Console.CmdArgs

constructQuery :: Query
constructQuery = QueryBoolQuery BoolQuery
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
