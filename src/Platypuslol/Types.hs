module Platypuslol.Types
  ( UsageTarget(..)
  , Command(..)
  , Commands
  , mkUrlRedirectCommand
  , mkCommands
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Types (status302)
import Network.Wai

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

data UsageTarget 
  = BrowserRedirect
  | ApplicationLauncher
  | ChatBot
  deriving (Show)

data Command = Command
  -- TODO: should use some fancy parsers
  { commandPrefixes :: [Text]
  , commandAction :: [Text] -> Response
  , commandTarget :: UsageTarget
  }

type Commands = HashMap Text Command

mkUrlRedirectCommand :: [Text] -> ([Text] -> Text) -> Command
mkUrlRedirectCommand prefixes action = Command
  { commandPrefixes = prefixes
  , commandTarget = BrowserRedirect
  , commandAction = \x -> responseBuilder
    status302
    [ ("Location"
      , encodeUtf8 $ action x
      )
    ]
    mempty
  }

mkCommands :: [Command] -> Commands
mkCommands = HashMap.fromList 
  . mconcat 
  . map 
    (\x -> map 
      (\y -> (y, x)) 
      (commandPrefixes x)
    )
