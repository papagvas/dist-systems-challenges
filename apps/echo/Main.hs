module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)

import Node

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  handleRequest
  forever . forkIO $ handleRequest
