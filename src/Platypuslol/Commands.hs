module Platypuslol.Commands
  ( commands
  , defaultCommand
  , queryParser
  , QueryLang(..)
  , mkSubstitutionQuery
  , SubstitutionQueries
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Data.Text (Text, pack)

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
  | QuerySubstitution String
  deriving (Show)

type SubstitutionQueries = HashMap String (AmbiguousParser (String, String))

-- TODO: use proper parser, allow escaping, but this is good start
toQueryLang :: String -> QueryLang
toQueryLang w@('{':_) = QueryString w
toQueryLang w@('[':_) = OptionalWord w
toQueryLang w@('<':_) = QueryWord w
toQueryLang w@('!':_) = QuerySubstitution w
toQueryLang w = PrefixWord w

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
    QuerySubstitution w -> suggestInstead (ParsedQuery [] (' ':w)) $ do
      ws <- space1
      query <- fromMaybe
        ((\x -> (x, x)) <$> word "<QUERY>")
        (HashMap.lookup w substitutions)
      pure ParsedQuery
        { parsedQuery = ws ++ fst query
        , parsedSubstitutions =
          [ Substitution
            { needle = pack w
            , replacement = pack $ snd query
            }
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
