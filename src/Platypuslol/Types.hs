module Platypuslol.Types
  ( Command
  , urlRedirect
  , mkCommand
  , Action(..)
  , ParsedCommand(..)
  ) where

import Data.List
import Data.Text (Text, replace, isPrefixOf)

import Platypuslol.AmbiguousParser
import Platypuslol.Util

data Action
  = UrlRedirect Text
  deriving (Show)

data ParsedCommand = ParsedCommand
  { parsedText :: String
  , parsedQueryParams :: Text
  , parsedAction :: Action
  }

-- TODO: remove strings
type Command = AmbiguousParser ParsedCommand

mkCommand
  :: AmbiguousParser (String, b)
  -> (b -> Text)
  -> (b -> Action)
  -> AmbiguousParser ParsedCommand
mkCommand commandWithParam showable fun = do
  cwp <- commandWithParam
  pure ParsedCommand
    { parsedText = fst cwp
    , parsedQueryParams = showable $ snd cwp
    , parsedAction = fun $ snd cwp
    }

-- TODO: this will probably have existing implementation
-- in the standard library.
-- TODO2: use haystack.
urlRedirect :: Text -> [(Text, Text)] -> Action
urlRedirect template replacements = UrlRedirect $ foldl'
  (\template' (haystack, replacement) -> replace
    haystack
    (encode haystack replacement)
    template'
  )
  template
  replacements
  where
    encode x = if "!" `Data.Text.isPrefixOf` x then id else urlEncodeText
