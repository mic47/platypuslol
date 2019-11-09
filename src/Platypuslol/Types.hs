{-# LANGUAGE DeriveAnyClass #-}
module Platypuslol.Types
  ( Command
  , urlRedirect
  , mkCommand
  , Action(..)
  , ParsedCommand(..)
  , ParsedQuery(..)
  , Substitution(..)
  ) where

import Data.Aeson
import GHC.Generics
import Data.List
import Data.Text (Text, replace, isPrefixOf)

import Platypuslol.AmbiguousParser
import Platypuslol.Util

data Action
  = UrlRedirect Text
  deriving (Show, Generic, ToJSON, FromJSON)

data ParsedCommand = ParsedCommand
  { parsedText :: String
  , parsedQueryParams :: Text
  , parsedAction :: Action
  } deriving (Show, Generic, ToJSON, FromJSON)

-- TODO: remove strings
type Command = AmbiguousParser ParsedCommand

data Substitution a = Substitution
  { needle :: a
  , replacement :: a
  }

data ParsedQuery a b = ParsedQuery
 { parsedSubstitutions :: [Substitution a]
 , parsedQuery :: b
 }

instance Functor (ParsedQuery a) where
  fmap f (ParsedQuery a b) = ParsedQuery a (f b)

mkCommand
  :: AmbiguousParser (ParsedQuery b String)
  -> ([Substitution b] -> Text)
  -> ([Substitution b] -> Action)
  -> AmbiguousParser ParsedCommand
mkCommand commandWithParam showable fun = do
  cwp <- commandWithParam
  pure ParsedCommand
    { parsedText = parsedQuery cwp
    , parsedQueryParams = showable $ parsedSubstitutions cwp
    , parsedAction = fun $ parsedSubstitutions cwp
    }

-- TODO: this will probably have existing implementation
-- in the standard library.
-- TODO2: use haystack.
urlRedirect :: Text -> [Substitution Text] -> Action
urlRedirect template replacements = UrlRedirect $ foldl'
  (\template' subst -> replace
    (needle subst)
    (encode (needle subst) (replacement subst))
    template'
  )
  template
  replacements
  where
    encode x = if "!" `Data.Text.isPrefixOf` x then id else urlEncodeText
