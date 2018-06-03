module Platypuslol.Types
  ( Command
  , urlRedirect
  , mkCommand
  , Action(..)
  ) where

import Data.List
import Data.Text (Text, replace, isPrefixOf)

import Platypuslol.AmbiguousParser
import Platypuslol.Util

data Action
  = UrlRedirect Text
  deriving (Show)

-- TODO: remove strings
type Command = AmbiguousParser (String, Text, Action)

mkCommand
  :: AmbiguousParser (a, b)
  -> (b -> s)
  -> (b -> c)
  -> AmbiguousParser (a, s, c)
mkCommand commandWithParam showable fun = do
  cwp <- commandWithParam
  pure (fst cwp, showable $ snd cwp, fun $ snd cwp)

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
