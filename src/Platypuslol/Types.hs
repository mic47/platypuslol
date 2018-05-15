module Platypuslol.Types
  ( Command
  , urlRedirect
  , mkCommand
  , Action(..)
  ) where

import Data.List
import Data.Text (Text, replace)

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
    (urlEncodeText replacement)
    template'
  )
  template
  replacements

