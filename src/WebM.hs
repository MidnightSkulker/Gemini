{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module WebM (
  WebM(..),
  webM,
  gets,
  gets2,
  modify ) where

import Control.Monad
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.IO.Class
import State (AppState)

-- WebM comes from https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs
--
-- Why 'ReaderT (TVar AppState)' rather than 'StateT AppState'?
-- With a state transformer, 'runActionToIO' (below) would have
-- to provide the state to _every action_, and save the resulting
-- state, using an MVar. This means actions would be blocking,
-- meaning only one request could be serviced at a time.
-- The 'ReaderT' solution means only actions that actually modify
-- the state need to block/retry.
--
-- Scotty requires the monad to be an instance of MonadIO
newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

-- Scotty's monads are layered on top of our custom monad.
-- We define this synonym for lift in order to be explicit
-- about when we are operating at the 'WebM' layer.
webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

-- Get the State
gets :: (AppState -> b) -> WebM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

-- Get 2 state components atomically
gets2 :: (AppState -> a) -> (AppState -> b) -> WebM (a,b)
gets2 f g = ask >>= liftIO . readTVarIO >>=
              return . (\appState -> (f appState, g appState))

-- Modify the State
-- Note that two modify functions can be composed resulting
-- in a sequence of modifications performed atomically.
modify :: (AppState -> AppState) -> WebM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f
