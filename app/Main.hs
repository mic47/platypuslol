module Main 
  ( main
  ) where
 
import Network.Wai.Handler.Warp

import qualified Platypuslol.Commands as PC
import Platypuslol.RedirectServer

main :: IO ()
main = do
  -- TODO: parse options
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run 
    port 
    (redirectServer PC.defaultCommand PC.commands)
 
