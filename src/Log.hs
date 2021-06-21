{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Log (
    Log(..),
    Entry(..),
    TransactionReport(..),
    emptyLog,
    transactionsHtml,
    addSendTransaction,
    addCreateTransaction,
    getTransactions,
    getAllTransactions ) where

import Data.Time.Clock
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Text.Encoding (decodeUtf8)
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

data Entry = SendEntry {
  amount :: Amount,
  toAddress :: Address,
  fromAddress :: Address,
  timestamp :: UTCTime }|
  CreateEntry {
  amount :: Amount,
  toAddress :: Address,
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
logLine entry =
  case entry of
    SendEntry amnt toA fromA timeS ->
      do
        tr $ do
          td $ text (pack (show (timestamp entry)))
          td $ text (pack (show (fromAddress entry)))
          td $ text (pack (show (toAddress entry)))
          td $ text (pack (show (amount entry)))
    CreateEntry amnt fromA timeS ->
      do
        tr $ do
          td $ text (pack (show (timestamp entry)))
          td $ text (pack (show (toAddress entry)))
          td $ text (pack (show (amount entry)))

addSendTransaction :: UTCTime -> Address -> Address -> Amount -> Log -> Log
addSendTransaction transAmount transToAddr transFromAddr transTime log =
  Log { entries = (SendEntry transTime transFromAddr transToAddr transAmount ):(entries log) }

addCreateTransaction :: UTCTime -> Address -> Amount -> Log -> Log
addCreateTransaction transAmount transToAddr transTime log =
  Log { entries = (CreateEntry transTime transToAddr transAmount ):(entries log) }

getTransactions :: Address -> Log -> [Entry]
getTransactions addr log =
  let matchAddress :: Address -> Entry -> Bool
      matchAddress addr entry =
        case entry of
          SendEntry amnt toA fromA timeS -> toA == addr || fromA == addr
          CreateEntry amnt toA timeS -> toA == addr
  in filter (matchAddress addr) (entries log)

-- For debugging
getAllTransactions :: Address -> Log -> [Entry]
getAllTransactions addr log = entries log
