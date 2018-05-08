module Platypuslol.Types
  ( UsageTarget(..)
  , Command(..)
  , singleParam
  , urlRedirect
  , mkCommand
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Network.HTTP.Types (status302)
import Network.Wai

import Data.Text (Text, replace, pack)
import Data.Text.Encoding (encodeUtf8)

import Platypuslol.AmbiguousParser
import Platypuslol.Util

data UsageTarget 
  = BrowserRedirect
  | ApplicationLauncher
  | ChatBot
  deriving (Show)

-- TODO: remove strings
type Command = AmbiguousParser (String, Text, Response)

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

urlRedirect :: Text -> Text -> Response
urlRedirect template query = responseBuilder
  status302
  [ ("Location"
    , encodeUtf8 $ replace
      "{query}"
      (urlEncodeText query)
      template
    )
  ]
  mempty
