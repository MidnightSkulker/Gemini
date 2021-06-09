{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Log where

import Data.Time.Clock
import Data.Aeson
import GHC.Generics
import qualified Data.Text as T

import Address
import Coins

data Entry = Entry {
  from :: Address,
  to :: Address,
  amount :: Amount,
  datetime :: UniversalTime
  } deriving (Generic, Show)

instance ToJSON UniversalTime where
  toJSON = String . T.pack . show

instance ToJSON Entry where
  toEncoding = genericToEncoding defaultOptions

type Log = [Entry]
