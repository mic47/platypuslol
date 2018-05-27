{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.STM
import Data.Monoid
import Data.Text (pack)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Applicative
import System.Directory
import System.FilePath.Posix
import System.FSNotify

import qualified Platypuslol.Commands as PC
import Platypuslol.CommandStore
import Platypuslol.RedirectServer
import Platypuslol.Types
import Paths_platypuslol

data Options = Options
  { localConfigDir :: FilePath
  , port :: Int
  , useTls :: Bool
  , tlsCertFile :: String
  , tlsKeyFile :: String
  }

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

loadCommandParser :: Options -> IO Command
loadCommandParser Options{..} = do
  localConfig <- (read <$> readFile (localConfigDir </> "commands.conf"))
    `catch` \(_ :: SomeException) -> (return [])
  localDB <- (read <$> readFile (localConfigDir </> "db.conf"))
    `catch` \(_ :: SomeException) -> (return [])
  globalConfigFile <- getDataFileName "resources/commands.conf"
  globalConfig <- (read <$> readFile globalConfigFile)
  return $ PC.commands $ localConfig ++ globalConfig ++ localDB

main :: IO ()
main = do
  opts@Options{..} <- parseOptions
  createDirectoryIfMissing True localConfigDir
  commandParser <- loadCommandParser opts
  commandStore <- newCommandStore commandParser
  putStrLn $ "Listening on port " ++ show port
  let defServer = "localhost:" <> pack (show port)
  withManager $ \fsNotify -> do
    void $ watchDir
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
