module Platypuslol.AmbiguousParser
  ( AmbiguousParser
  , parseAll
  , char
  , string
  , prefix
  , option
  , anyOf
  , many
  , many1
  , eatAll
  , word
  , anyString
  , prefixSentence
  , prefixSentence'
  , spaced
  , separated
  ) where

import Control.Arrow
import Data.List

newtype AmbiguousParser a = AmbiguousParser { parse :: String -> [(a, String)]}

parseAll :: (AmbiguousParser a) -> String -> [a]
parseAll (AmbiguousParser p) = map fst . filter ((""==) . snd) . p

instance Functor AmbiguousParser where
  fmap f (AmbiguousParser p) = AmbiguousParser $ map (\(a, x)-> (f a, x)) . p

instance Applicative AmbiguousParser where
  pure x = AmbiguousParser $ \a -> [(x, a)]
  (AmbiguousParser pf) <*> (AmbiguousParser px) = AmbiguousParser $ \s ->
    [ (f x, rest2)
    | (f, rest1) <- pf s
    , (x, rest2) <- px rest1
    ]

option :: AmbiguousParser a -> AmbiguousParser a -> AmbiguousParser a
option p q = anyOf [p, q]

char :: Char -> AmbiguousParser Char
char c = AmbiguousParser
  (\case
    x:xs | x == c -> [(x, xs)]
    _ -> []
  )

string :: String -> AmbiguousParser String
string [] = pure []
string (x:xs) = do
  x' <- char x
  xs' <- string xs
  return $ x':xs'

anyOf :: [AmbiguousParser a] -> AmbiguousParser a
anyOf parsers = AmbiguousParser $ \s -> mconcat
  ( map
    (\p -> parse p s)
    parsers
  )

many :: AmbiguousParser a -> AmbiguousParser [a]
many parser = option
  (pure [])
  (many1 parser)

many1 :: AmbiguousParser a -> AmbiguousParser [a]
many1 parser = do
  x <- parser
  y <- option
    (pure [])
    (many1 parser)
  pure (x:y)

prefix :: String -> AmbiguousParser String
prefix x = anyOf $ map (fmap (const x) . string) $ reverse $ drop 1 $ inits x

eatAll :: AmbiguousParser String
eatAll = AmbiguousParser $ \x -> [(x, "")]

word :: AmbiguousParser String
word = AmbiguousParser $ \x ->
  [( takeWhile (/= ' ') x
  , drop 1 $ dropWhile (/= ' ') x
  )]


anyString :: AmbiguousParser String
anyString = AmbiguousParser splits

splits :: [a] -> [([a], [a])]
splits [] = []
splits (a:as) = ([a], as) : (map (first (a:)) $ splits as)

prefixSentence :: [String] -> AmbiguousParser [String]
prefixSentence [] = pure []
prefixSentence (x:[]) = fmap (\y -> [y]) $ prefix x
prefixSentence (x:xs) = do
  x' <- prefix x
  w <- many $ string " "
  xs' <- prefixSentence xs
  pure $ const (x':xs') w

prefixSentence' :: String -> AmbiguousParser String
prefixSentence' = fmap (mconcat . intersperse " ") . prefixSentence . words

spaced :: AmbiguousParser a -> AmbiguousParser b -> AmbiguousParser (a, b)
spaced p1 p2 = do
  p1' <- p1
  w <- many1 $ char ' '
  p2' <- p2
  pure $ const (p1', p2') w

separated :: AmbiguousParser a -> [AmbiguousParser b] -> AmbiguousParser [b]
separated _ [] = pure []
separated _ [x] = fmap (\a -> [a]) x
separated separator (x:xs) = do
  x' <- x
  s <- separator
  xs' <- separated separator xs
  pure $ const (x':xs') s
