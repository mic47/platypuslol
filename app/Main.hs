{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.List
import qualified Data.HashMap.Strict as HashMap
import Data.Text (pack)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Directory
import System.FilePath.Posix
import System.FSNotify

import qualified Platypuslol.Commands as PC
import Platypuslol.AmbiguousParser (anyOf)
import Platypuslol.CommandStore
import Platypuslol.Commands
import Platypuslol.RedirectServer
import Platypuslol.Types
import Paths_platypuslol

data Options = Options
  { localConfigDir :: FilePath
  , port :: Int
  , useTls :: Bool
  , tlsCertFile :: String
  , tlsKeyFile :: String
  } deriving (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
    ( long "config-dir"
    <> metavar "FILE"
    <> value "/home/mic/.config/platypuslol"
    )
  <*> option auto
    ( long "port"
    <> metavar "PORT"
    <> value 3000
    )
  <*> switch
    ( long "use-tls"
    )
  <*> strOption
    ( long "tls-cert-file"
    <> metavar "CERTFILE"
    <> value "localhost.crt"
    )
  <*> strOption
    ( long "tls-key-file"
    <> metavar "KEYFILE"
    <> value "localhost.key"
    )

parseOptions :: IO Options
parseOptions = execParser
  (info (optionsParser <**> helper)
    ( fullDesc
    <> progDesc "Platypuslol: smart bookmars, search and redirects."
    )
  )

loadSubstitutions :: Options -> IO PC.SubstitutionQueries
loadSubstitutions Options{..} = do
  files <- filter (isSuffixOf ".conf") <$> listDirectory substDir
  HashMap.map
    ( fmap
      (\x ->
        SubstitutionQuery
        { searchedValue = fst x
        , replacements = [Substitution {needle = "", replacement = snd x}]
        }
      )
    )
    . HashMap.fromList
    <$> mapM loadFile files
  where
    substDir = localConfigDir </> "substitutions"
    loadFile filename = do
      subst <- read <$> readFile (substDir </> filename)
      return
        ( "!" ++ fst (splitExtension filename) ++ "!"
        , anyOf $ map PC.mkSubstitutionQuery subst
        )

loadCommandParser :: Options -> IO Command
loadCommandParser opts@Options{..} = do
  localConfig <- read <$> readFile (localConfigDir </> "commands.conf")
    `catch` \(_ :: SomeException) -> (return [])
  localDB <- read <$> readFile (localConfigDir </> "db.conf")
    `catch` \(_ :: SomeException) -> (return [])
  globalConfigFile <- getDataFileName "resources/commands.conf"
  globalConfig <- read <$> readFile globalConfigFile
  substitutions <- loadSubstitutions opts
  return $ PC.commands
    (localConfig ++ globalConfig ++ localDB)
    substitutions

main :: IO ()
main = do
  opts@Options{..} <- parseOptions
  createDirectoryIfMissing True localConfigDir
  commandParser <- loadCommandParser opts
  commandStore <- newCommandStore commandParser
  putStrLn $ "Listening on port " ++ show port
  let defServer = "localhost:" <> pack (show port)
  withManager $ \fsNotify -> do
    void $ watchTree
      fsNotify
      localConfigDir
      (const True)
      $ const $ do
        cmd <- loadCommandParser opts
        atomically $ setCommandParser commandStore cmd
        putStrLn "Reloaded parsers."

    if useTls
    then runTLS
      ( tlsSettings
        tlsCertFile
        tlsKeyFile
      )
      (setPort port defaultSettings)
      (redirectServer
        PC.defaultCommand
        ("https://", defServer)
        commandStore
        localConfigDir
      )
    else run
      port
      (redirectServer
        PC.defaultCommand
        ("http://", defServer)
        commandStore
        localConfigDir
      )
