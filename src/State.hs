{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module State (AppState(..), initAppState) where

import Log
import Coins
import Data.Aeson
import GHC.Generics

-- The global state for the Server
data AppState = AppState
  { applog :: Log,
    ledger :: Ledger,
    tickCount :: Int -- For trying out ideas
  } deriving (Show, Generic, ToJSON)

initAppState = AppState {
  applog = emptyLog,
  ledger = emptyLedger,
  tickCount = 0 }
