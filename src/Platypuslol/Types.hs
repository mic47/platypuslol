module Platypuslol.Types
  ( Command
  , singleParam
  , urlRedirect
  , mkCommand
  , Action(..)
  ) where

import Data.Text (Text, replace, pack)

import Platypuslol.AmbiguousParser
import Platypuslol.Util

data Action 
  = UrlRedirect Text
  deriving (Show)

-- TODO: remove strings
type Command = AmbiguousParser (String, Text, Action)

singleParam :: AmbiguousParser a -> AmbiguousParser (a, Text)
singleParam = fmap (fmap pack) . (`spaced` eatAll)

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
urlRedirect template replacements = UrlRedirect $ foldl
  (\template' (haystack, replacement) -> replace
    haystack
    (urlEncodeText replacement)
    template'
  )
  template
  replacements

