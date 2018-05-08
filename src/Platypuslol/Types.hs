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

urlRedirect :: Text -> Text -> Action
urlRedirect template query = UrlRedirect $ replace
  "{query}"
  (urlEncodeText query)
  template

