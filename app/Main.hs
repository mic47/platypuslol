{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  ) where

import Control.Exception
import Data.Monoid
import Data.Text (pack)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Applicative

import qualified Platypuslol.Commands as PC
import Platypuslol.RedirectServer

data Options = Options
  { localConfigFile :: FilePath
  , port :: Int
  , useTls :: Bool
  , tlsCertFile :: String
  , tlsKeyFile :: String
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
    ( long "config"
    <> metavar "FILE"
    <> value "/home/mic/.platypus.conf"
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

main :: IO ()
main = do
  Options{..} <- parseOptions
  config <- (read <$> readFile localConfigFile)
    `catch` \(_ :: SomeException) -> (return [])
  putStrLn $ "Listening on port " ++ show port
  let defServer = "localhost" <> pack (show port)
  if useTls
    then runTLS
      ( tlsSettings
        tlsCertFile
        tlsKeyFile
      )
      (setPort port defaultSettings)
      (redirectServer 
        PC.defaultCommand 
        ("https://" <> defServer) 
        (PC.commands config)
      )
    else run
      port
      (redirectServer 
        PC.defaultCommand 
        ("http://" <> defServer) 
        (PC.commands config)
      )
