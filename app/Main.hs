module Main 
  ( main
  ) where
 
import Control.Exception
import Network.Wai.Handler.Warp

import qualified Platypuslol.Commands as PC
import Platypuslol.RedirectServer

main :: IO ()
main = do
  -- TODO: parse options
  config <- read <$> readFile "/home/mic/.platypus.config"
    `catch` \(_ :: SomeException) -> (return [])
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run 
    port 
    (redirectServer PC.defaultCommand $ PC.commands config)
 
