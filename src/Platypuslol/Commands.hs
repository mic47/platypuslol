module Platypuslol.Commands
  ( commands
  , defaultCommand
  ) where

import Data.Monoid

import Platypuslol.Types
import Platypuslol.Util

-- | g, or google is for searching google
defaultCommand :: Command
defaultCommand = mkUrlRedirectCommand
  ["g", "google"]
  (\query -> 
    "https://www.google.sk/search?q=" 
    <> urlEncodeQuery (drop 1 query)
  )

commands :: Commands
commands = mkCommands $ 
  [ defaultCommand
  -- hoo for hoogle on stackage.
  , mkUrlRedirectCommand
    ["hoo", "hoogle"]
    (\query -> 
      "https://www.stackage.org/lts-10.8/hoogle?q=" 
      <> urlEncodeQuery (drop 1 query)
    )
  -- loo for local hoogle
  , mkUrlRedirectCommand
    ["loo", "loogle"]
    (\query ->
      "http://localhost:8080/?hoogle="
      <> urlEncodeQuery (drop 1 query)
    )
  ]
