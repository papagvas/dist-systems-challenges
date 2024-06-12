module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)

import Node

main :: IO ()
main = forever $ do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  handleRequest
  forkIO $ handleRequest
