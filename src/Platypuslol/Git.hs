{-# LANGUAGE DeriveAnyClass #-}
module Platypuslol.Git where

import Data.Aeson
import GHC.Generics
import Data.Char
import Data.List
import Data.Maybe
import Data.List.Extra
import System.Process

import Platypuslol.Types
import Platypuslol.Util

data GitRepo = GitRepo
  { remoteUrl :: String
  , localDirectory :: String
  , name :: String
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data GitBranch = GitBranch
  { repo :: GitRepo
  , branch :: String
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

remoteUrlToGithubUrl :: String -> Maybe String
remoteUrlToGithubUrl rUrl = ("https://github.com/" <>) <$> remoteUrlToGithubUserAndNameStr rUrl

remoteUrlToGithubUserAndNameStr :: String -> Maybe String
remoteUrlToGithubUserAndNameStr rUrl = stripSuffix ".git" =<< stripPrefix "git@github.com:" rUrl

remoteUrlToRepoName :: String -> String
remoteUrlToRepoName = drop 1 . dropWhile (/= '/') . fromMaybe "" . remoteUrlToGithubUserAndNameStr

extractRepoInfo :: FilePath -> IO GitRepo
extractRepoInfo filePath = do
  remoteUrlStr <-
    strip isSpace
    <$> readCreateProcess remoteUrlCommand ""
  pure GitRepo
    { name = remoteUrlToRepoName remoteUrlStr
    , localDirectory = filePath
    , remoteUrl = remoteUrlStr
    }
  where
  remoteUrlCommand :: CreateProcess
  remoteUrlCommand = (shell "git config --get remote.origin.url") { cwd = Just filePath}

extractBranches :: Bool -> FilePath -> IO [GitBranch]
extractBranches remote filePath = do
  repo <- extractRepoInfo filePath
  branches <-
    filter (/= "")
    . map (\x -> fromMaybe x (stripPrefix "origin/" x))
    . map (strip (\x -> x == '*' || isSpace x))
    . filter (not . isInfixOf " -> ")
    . lines
    <$> readCreateProcess (branchCommand remote) ""
  pure
    [ GitBranch
      { repo = repo
      , branch = b
      }
      | b <- branches
    ]
  where
  branchCommand :: Bool -> CreateProcess
  branchCommand True = (shell "git branch --list --remote") { cwd = Just filePath}
  branchCommand False = (shell "git branch --list") { cwd = Just filePath}

-- TODO: Make generic deriving for this stuff
instance ToSubstitutions GitRepo where
  toSubstitutions repo = addGhUrl
    [ Substitution
      { needle = "remoteUrl"
      , replacement = remoteUrl repo
      }
    , Substitution
      { needle = "localDirectory"
      , replacement = localDirectory repo
      }
    , Substitution
      { needle = "name"
      , replacement = name repo
      }
    ]
    where
    addGhUrl x = case remoteUrlToGithubUrl (remoteUrl repo) of
      Nothing -> x
      Just gh -> Substitution {needle = "githubUrl", replacement = gh}: x

instance ToSubstitutions GitBranch where
  toSubstitutions b =
    Substitution
    { needle = "branch"
    , replacement = branch b
    }: Substitution
    { needle = "repo/branch"
    , replacement = mconcat
      [ (name . repo) b
      , "/"
      , branch b
      ]
    }: [s {needle = "repo." <> needle s} | s <- toSubstitutions (repo b)]
