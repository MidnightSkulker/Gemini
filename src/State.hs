{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ScopedTypeVariables #-}
module State (
  AppState(..),
  initAppState,
  getAppValue,
  add50) where

import Log
import Address
import Coins
import Debug.Trace
import Data.Aeson
import GHC.Generics

-- The global state for the Server
data AppState = AppState
  { appLog :: Log,
    appLedger :: Ledger,
    tickCount :: Int -- For trying out ideas
  } deriving (Show, Generic, ToJSON)

initAppState = AppState {
  appLog = emptyLog,
  appLedger = emptyLedger,
  tickCount = 0 }

-- Add coins to a ledger
add50 :: Address -> AppState -> AppState
add50 addr appState = appState { appLedger = addValue 50.0 addr (appLedger appState)}

getAppValue :: Address -> AppState -> Amount
getAppValue addr appState = getValue addr (appLedger appState)

addAppTransaction :: UniversalTime -> Address -> Address -> Amount -> AppState -> AppState
addAppTransaction currentTime transFromAddr transToAddr transAmount appState =
  appState { appLog = addTransaction currentTime transFromAddr transToAddr transAmount (appLog appState) }
