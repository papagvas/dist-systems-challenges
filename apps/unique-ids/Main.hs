module Main where

import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Monad (forever)
import System.IO (hSetBuffering, BufferMode(..), stdin, stdout)

import Node

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  chan <- newChan
  _ <- handleInit
  _ <- forkIO . forever $ do
    msg <- receive
    writeChan chan msg
  forever $ do
    msg' <- readChan chan
    respond msg'
