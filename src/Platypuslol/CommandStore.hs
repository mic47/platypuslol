module Platypuslol.CommandStore
  ( CommandStore
  , getCommandParser
  , newCommandStore
  ) where

import Control.Monad.STM
import Control.Concurrent.STM

import Platypuslol.Types

newtype CommandStore = CommandStore (TVar Command)

getCommandParser :: CommandStore -> STM Command
getCommandParser (CommandStore x) = readTVar x

newCommandStore :: Command -> IO CommandStore
newCommandStore = fmap CommandStore . newTVarIO
