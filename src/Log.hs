{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Log (
    Log(..),
    emptyLog,
    transactionsHtml,
    addTransaction) where

import Data.Time.Clock
import Data.Aeson
import GHC.Generics
import Text.Blaze.Html
import Text.Blaze.XHtml1.FrameSet
import Text.Blaze.Html5.Attributes hiding (datetime)
import qualified Data.Text as T
import Data.Text hiding (length, head)
import qualified Data.Map as Map
import Data.Foldable (foldlM)

import Address
import Coins (Amount)

data Entry = Entry {
  fromAddr :: Address,
  toAddr :: Address,
  amount :: Amount,
  datetime :: UTCTime
  } deriving (Generic, Show, ToJSON)

-- Only thing compiler cannot derive for ToJSON instance
-- instance ToJSON UTCTime where
--   toJSON = String . T.pack . show

-- The log is a list of log entries
-- Normally a log would be done using some logging facility
-- such as log4j, I wanted to show some skills coding business
-- logic and HTML formatting here for the coding challenge.
data Log = Log {entries :: [Entry]} deriving (Generic, Show, ToJSON)

emptyLog :: Log
emptyLog = Log { entries = [] }

-- Transactions to HTML
transactionsHtml :: Log -> Html
transactionsHtml log =
  let numberOfLogs :: Int = length (entries log)
      numberOfLogsHtml :: Html = text (pack (show (numberOfLogs) ++ " Transactions"))
  in do
    h3 ! class_ "ui header" $ "Transactions"
    table ! class_ "ui collapsing table segment" $ do
      thead $ do
        tr $ do
          th $ "Timestamp"
          th $ "From"
          th $ "To"
          th $ "Amount"
      tbody $ do
        foldMap logLine (entries log)
      tfoot $ do
        tr $ do
          td ! colspan "2" $ numberOfLogsHtml

logLine :: Entry -> Html
logLine entry = do
    tr $ do
      td $ text (pack (show (fromAddr entry)))
      td $ text (pack (show (toAddr entry)))
      td $ text (pack (show (amount entry)))
      td $ text (pack (show (datetime entry)))

addTransaction :: UTCTime -> Address -> Address -> Amount -> Log -> Log
addTransaction  transTime transFromAddr transToAddr transAmount log =
  Log { entries = (Entry transFromAddr transToAddr transAmount transTime):(entries log) }
