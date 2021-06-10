{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Coins (
  Coin,
  Amount,
  Ledger(..),
  LedgerLine(..),
  ledgerLine,
  ledger2html,
  emptyLedger,
  addValue,
  getValue ) where

import Prelude hiding (div, head)
import Address
import Data.Aeson
import GHC.Generics
-- Imports for formatting HTML
import Text.Blaze.Html
import Text.Blaze.XHtml1.FrameSet
import Text.Blaze.Html5.Attributes
import Data.Text hiding (length, head)
import qualified Data.Map as Map

type Coin = Address
type Amount = Float

data LedgerLine = LedgerLine
  { user :: Address,
    amount :: Amount
  } deriving (Show, Generic, ToJSON)

type LedgerEntry = (Address, Amount)

-- The requirements of the coding demo is that the ledger needs
-- to be displayable on a web page. So our ledger will have
-- only a small number of entries, not thousands or millions of
-- entries. Nevertheless, we will consider efficient of lookups
-- and updates in the ledger by implementing it as a Map (i.e. a
-- hash table), not a list. That way updates and searches will
-- take nearly constant time.
data Ledger =
  Ledger {items :: Map.Map Address Amount} deriving (Show, Generic, ToJSON)

-- Format a ledger entry for HTML
ledgerLine :: LedgerEntry -> Html
ledgerLine l = do
  tr $ do
    td (a (text (pack("href=\"/pout-jersey/addresses/\"") `append` (pack (fst l)))))
    td (text (pack (show (snd l))))

-- Format the table header
tableHeader :: Text -> Text -> Html
tableHeader col1hdr col2hdr = do
  thead $ do
    tr $ do
      th (text col1hdr)
      th (text col2hdr)

-- Format the legder table for Html
-- mapM :: (Taversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- For formatting the web page, I must assume the ledger does not
-- have a zillion entries, so that it is reasonable to Send
-- the ledger using http and displaying it on your browser.
-- This I convert the ledger to an association list in preparation
-- for display.
ledger2html :: Ledger -> Html
ledger2html ledger =
  let numberOfItems :: Int = length (items ledger)
      numberOfItemsHtml :: Html =
        if numberOfItems == 0
        then text "No Entries"
        else text (pack (show (numberOfItems) ++ " Addresses"))
  in do
      h3 ! class_ "ui header" $ "Current Balances"
      table ! class_ "ui collapsing table segment" $ do
        tableHeader "Address" "Balance"
        tbody $ do
          mapM ledgerLine (Map.assocs (items ledger))
          tfoot $ do
            tr $ do
              td ! colspan "2" $ numberOfItemsHtml

-- Initial value of the Ledger
emptyLedger :: Ledger
emptyLedger = Ledger { items = Map.empty }

-- Add value to a user in the ledger
-- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
addValue :: Amount -> Address -> Ledger -> Ledger
addValue amount addr ledger =
  Ledger { items = Map.adjust (\x -> x + 50.0) addr (items ledger) }

-- findWithDefault :: Ord k => a -> k -> Map k a -> a
getValue :: Address -> Ledger -> Amount
getValue addr ledger = Map.findWithDefault 0.0 addr (items ledger)
