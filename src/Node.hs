module Node where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Lens.Micro

import Protocol

handleRequest :: IO ()
handleRequest = do
  msg <- BS.getLine
  case eitherDecodeStrict @Message msg of
    Left errorMsg -> fail errorMsg
    Right msg' -> respond msg'

respond :: Message -> IO ()
respond msg = case msg ^. #body . #payload of
  Init _ _ -> send MkMessage { src  = msg ^. #dest
                             , dest = msg ^. #src
                             , body = MkBody { inReplyTo = msg ^. #body . #msgId
                                             , msgId     = Nothing
                                             , payload   = InitOk 
                                             }
                             }
  Echo echo -> send MkMessage { src  = msg ^. #dest
                              , dest = msg ^. #src
                              , body = MkBody { inReplyTo = msg ^. #body . #msgId
                                              , msgId     = msg ^. #body . #msgId
                                              , payload   = EchoOk echo
                                              }
                              }
  _ -> return ()

send :: Message -> IO ()
send = BL.putStrLn . encode