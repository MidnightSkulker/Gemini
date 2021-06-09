{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Coins (
  Coin,
  Amount,
  Ledger(..),
  LedgerLine(..),
  ledgerLine,
  ledger2html,
  emptyLedger ) where

import Address
import Data.Aeson
import GHC.Generics
-- Imports for formatting HTML
import Text.Blaze.Html
import Text.Blaze.XHtml1.FrameSet
import Text.Blaze.Html5.Attributes
import Data.Text hiding (length)

type Coin = Address
type Amount = Float

data LedgerLine = LedgerLine
  { user :: Address,
    amount :: Amount
  } deriving (Show, Generic, ToJSON)

data Ledger =
  Ledger {items :: [LedgerLine]} deriving (Show, Generic, ToJSON)

-- Format a ledger entry for HTML
ledgerLine :: LedgerLine -> Html
ledgerLine l = do
  tr $ do
    td (a (text (pack("href=\"/pout-jersey/addresses/\"") `append` (pack (user l)))))
    td (text (pack (show (amount l))))

-- Format the table header
tableHeader :: Text -> Text -> Html
tableHeader col1hdr col2hdr = do
  thead $ do
    tr $ do
      th (text col1hdr)
      th (text col2hdr)

-- Format the legder table for Html
-- mapM :: (Taversable t, Monad m) => (a -> m b) -> t a -> m (t b)
ledger2html :: Ledger -> Text -> Html
ledger2html ledger addr =
  let numberOfItems :: Int = length (items ledger)
      numberOfItemsHtml :: Html =
        if numberOfItems == 0
        then text "No Entries"
        else text (pack (show (numberOfItems) ++ " Addresses"))
  in do
      table ! class_ "ui collapsing table segment" $ do
        tableHeader "Address" "Balance"
        tbody $ do
          mapM ledgerLine (items ledger)
          tfoot $ do
            tr $ do
              td ! colspan "2" $ numberOfItemsHtml

emptyLedger :: Ledger
emptyLedger = Ledger { items = [] }
