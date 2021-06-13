{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Log (
    Log(..),
    Entry(..),
    TransactionReport(..),
    emptyLog,
    transactionsHtml,
    addTransaction,
    getTransactions,
    getAllTransactions ) where

import Data.Time.Clock
import Data.Aeson
import GHC.Generics
import Text.Blaze.Html
import Text.Blaze.XHtml1.FrameSet
import Text.Blaze.Html5.Attributes hiding (datetime)
import qualified Data.Text.Lazy as L
import Data.Text hiding (length, head, filter)
import qualified Data.Map as Map hiding (filter)
import Data.Foldable (foldlM)

import Address
import Coins (Amount)

data Entry = Entry {
  amount :: Amount,
  toAddress :: Address,
  fromAddress :: Address,
  timestamp :: UTCTime
  } deriving (Generic, Show, ToJSON)

data TransactionReport = TransactionReport {
  balance :: Amount,
  transactions :: [Entry]
  } deriving (Generic, Show, ToJSON)

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
      td $ text (pack (show (timestamp entry)))
      td $ text (pack (show (fromAddress entry)))
      td $ text (pack (show (toAddress entry)))
      td $ text (pack (show (amount entry)))

addTransaction :: UTCTime -> Address -> Address -> Amount -> Log -> Log
addTransaction transAmount transToAddr transFromAddr transTime log =
  Log { entries = (Entry transTime transFromAddr transToAddr transAmount ):(entries log) }

getTransactions :: Address -> Log -> [Entry]
getTransactions addr log =
  filter (\e -> toAddress e == addr || fromAddress e == addr) (entries log)

-- For debugging
getAllTransactions :: Address -> Log -> [Entry]
getAllTransactions addr log = entries log
