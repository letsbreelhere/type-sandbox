module Main where

import Control.Monad.State
import Dependent.Command
import Dependent.Parsing
import Util.Repl

main :: IO ()
main = evalStateT replLoop defaultReplState

replLoop :: StateT ReplState IO ()
replLoop = repl "π> " $ either pure process . parseCommand
