{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util.Repl where

import Control.Monad.Trans (lift)
import Control.Monad.State
import System.Console.Haskeline

-- Orphan instance because Haskeline uses `transformers` instead of `mtl`.
-- Code stolen directly from http://hackage.haskell.org/package/haskeline-0.7.4.0/docs/src/System-Console-Haskeline-MonadException.html#line-152
instance (MonadException m) => MonadException (StateT s m) where
  controlIO f = StateT $ \s -> controlIO $ \(RunIO run) ->
    let run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
     in flip runStateT s <$> f run'

repl :: (MonadException m) => String -> (String -> m String) -> m ()
repl prompt process = runInputT defaultSettings loop
  where loop = do
          minput <- getInputLine prompt
          case minput of
            Nothing -> pure ()
            Just input -> lift (process input) >>= outputStrLn >> loop
