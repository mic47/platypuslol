{-# LANGUAGE DeriveAnyClass #-}
module Platypuslol.Commands
  ( commands
  , defaultCommand
  , queryParser
  , QueryLang(..)
  , mkSubstitutionQuery
  , substitutionQueryParser
  , SubstitutionQueries
  , SubstitutionQuery(..)
  , toSubstitutionQuery
  ) where

import Data.Aeson
import GHC.Generics
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Data.Text (Text, pack)
import qualified Text.ParserCombinators.Parsec as P

import Platypuslol.AmbiguousParser
import Platypuslol.Types

defaultCommand :: Text -> Action
defaultCommand query = urlRedirect
  "https://www.google.com/search?q={query}"
  [ Substitution
    { needle = "{query}"
    , replacement = query
    }
  ]

-- TODO: maybe add non-prefix words?
data QueryLang
  = PrefixWord String
  | OptionalWord String
  | QueryString String
  | QueryWord String
  | QuerySubstitution String String
  deriving (Show)


-- Type, subtype, value
data SubstitutionQuery = SubstitutionQuery
  { searchedValue :: String
  , replacements :: [Substitution String]
  } deriving (Show, Generic, ToJSON, FromJSON)

type SubstitutionQueries = HashMap String (AmbiguousParser SubstitutionQuery)

substitutionQueryParser :: SubstitutionQuery -> AmbiguousParser SubstitutionQuery
substitutionQueryParser q = fmap (const q) (subsequenceWord (searchedValue q))

toSubstitutionQuery
  :: (ToSubstitutions a)
  => String
  -> a
  -> Maybe SubstitutionQuery
toSubstitutionQuery key x = case listToMaybe (filter (\y -> key == needle y) repls) of
  Nothing -> Nothing
  Just val -> Just SubstitutionQuery
    { searchedValue = replacement val
    , replacements = repls
    }
  where
  repls = toSubstitutions x

-- TODO: use proper parser, allow escaping, but this is good start
toQueryLang :: String -> QueryLang
toQueryLang w@('{':_) = QueryString w
toQueryLang w@('[':_) = OptionalWord w
toQueryLang w@('<':_) = QueryWord w
toQueryLang w@('!':_) = case P.parse substQueryParser "" w of
  Right ret -> ret
  Left _ -> PrefixWord w
toQueryLang w = PrefixWord w

substQueryParser :: P.GenParser Char st QueryLang
substQueryParser = do
  type_ <- P.char ('!' :: Char) *> P.many (P.noneOf "!:")
  name <- P.optionMaybe $ do
    _ <- P.char ':'
    P.many (P.noneOf "!")
  _ <- P.char '!'
  pure $ QuerySubstitution ("!" <> type_ <> "!") (fromMaybe type_ name)

queryParser :: SubstitutionQueries -> [QueryLang] -> AmbiguousParser (ParsedQuery Text [String])
queryParser _ [] = pure $ ParsedQuery [] []
queryParser substitutions (q:qs) = do
  parse <- case q of
    PrefixWord w -> ParsedQuery [] <$> prefix w
    OptionalWord w -> ParsedQuery [] <$> anyOf [string w, pure ""]
    QueryWord w -> do
      query <- word "<WORD>"
      pure ParsedQuery
        { parsedQuery = query
        , parsedSubstitutions =
          [ Substitution
            { needle = pack w
            , replacement = pack query
            }
          ]
        }
    QueryString w -> do
      query <- case qs of
        [] -> eatAll "<QUERY>"
        _ -> anyString "<QUERY>"
      pure ParsedQuery
        { parsedQuery = query
        , parsedSubstitutions =
          [ Substitution
            { needle = pack w
            , replacement = pack query
            }
          ]
        }
    QuerySubstitution type_ name -> suggestInstead (ParsedQuery [] (' ':name)) $ do
      ws <- space1
      query <- fromMaybe
        ((\x -> SubstitutionQuery x [Substitution "" x]) <$> word "<QUERY>")
        (HashMap.lookup type_ substitutions)
      pure ParsedQuery
        { parsedQuery = ws ++ searchedValue query
        , parsedSubstitutions =
          [ Substitution
            { needle = pack (mconcat ["!", name, ".", needle s, "!"])
            , replacement = pack $ replacement s
            }
          | s <- replacements query
          ]
        }
  _ <- spaceType
  parse' <- queryParser substitutions qs
  pure ParsedQuery
    { parsedQuery = parsedQuery parse:parsedQuery parse'
    , parsedSubstitutions = parsedSubstitutions parse ++ parsedSubstitutions parse'
    }
    where
      spaceType = case (q, qs) of
        -- There is always space before query.
        (_, QueryString{}:_) -> space1
        (_, QueryWord{}:_) -> space1
        (_, QuerySubstitution{}:_) -> pure ""
        -- There is always space after query, if non-query follows.
        (QueryWord{}, _:_) -> space1
        (QueryString{}, _:_) -> space1
        (QuerySubstitution{}, _:_) -> space1
        _ -> space

queryParser' :: SubstitutionQueries -> [QueryLang] -> AmbiguousParser (ParsedQuery Text String)
queryParser' substitutions = fmap (fmap (mconcat . intersperse " ")) . queryParser substitutions

simpleRedirect :: SubstitutionQueries -> String -> Text -> Command
simpleRedirect substitutions command redirectTemplate = mkCommand
  (queryParser' substitutions $ map toQueryLang $ words command)
  (mconcat . intersperse "|" . map replacement)
  (urlRedirect redirectTemplate)

mkSubstitutionQuery :: (String, String) -> AmbiguousParser (String, String)
mkSubstitutionQuery x = fmap (const x) (subsequenceWord $ fst x)

commands :: [(String, Text)] -> SubstitutionQueries -> Command
commands config subs = anyOf $ map (uncurry $ simpleRedirect subs) config
