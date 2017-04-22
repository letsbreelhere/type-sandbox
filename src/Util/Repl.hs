module Util.Repl where

import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline

repl :: String -> (String -> IO String) -> IO ()
repl prompt process = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine prompt
          case minput of
            Nothing -> pure ()
            Just input -> liftIO (process input) >>= outputStrLn >> loop
