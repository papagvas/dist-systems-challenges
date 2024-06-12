module Node where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.String (fromString)
import Data.Text (Text)
import Lens.Micro

import Protocol

handleInit :: IO NodeId
handleInit = do
  msg <- BS.getLine
  case eitherDecodeStrict @Message msg of
    Left errorMsg -> fail errorMsg
    Right initMsg -> case initMsg ^. #body  . #payload of
      Init{..} -> do
        respond initMsg
        return nodeId
      _ -> fail "Not init msg"


receive :: IO Message
receive = do
  input <- BS.getLine
  case eitherDecodeStrict @Message input of
    Left errorMsg -> fail errorMsg
    Right msg -> return msg

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
                                              , msgId     = Nothing
                                              , payload   = EchoOk echo
                                              }
                              }
  Generate -> send MkMessage { src  = msg ^. #dest
                             , dest = msg ^. #src
                             , body = MkBody { inReplyTo = msg ^. #body . #msgId
                                             , msgId = Nothing
                                             , payload = GenerateOk uid
                                             }
                             }
    where
      getUid :: NodeId -> Maybe MsgId -> Text
      getUid (MkNodeId num) (Just (MkMsgId txt)) = mconcat [ num
                                                           , "-"
                                                           , fromString $ show txt
                                                           ]
      getUid (MkNodeId num) _ = fromString $ show num ++ "it will work, right?"

      nid = msg ^. #dest
      mid = msg ^. #body . #msgId
      uid = getUid nid mid

  _ -> return ()

send :: Message -> IO ()
send = BL.putStrLn . encode