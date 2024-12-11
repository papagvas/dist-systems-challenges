module Node where

import Control.Monad.Reader
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.IORef
import Data.String (fromString)
import Data.Text (Text)
import Lens.Micro

import Protocol
import Data.IORef (atomicModifyIORef')

type Node = ReaderT Env IO

runNode :: Node a -> Env -> IO a
runNode = runReaderT

createEnv :: MonadIO m => m Env
createEnv = do
  nid <- handleInit
  msgs <- liftIO $ newIORef []
  return $ MkEnv nid msgs

data Env = MkEnv { nid  :: NodeId
                 , msgs :: IORef [BCMsg]
                 }

class HasMessages a where
  getMsgs :: a -> IORef [BCMsg]

instance HasMessages Env where
  getMsgs = msgs

handleInit :: MonadIO m => m NodeId
handleInit = do
  initMsg <- receive
  case initMsg ^. #body . #payload of
      Init{..} -> do
        send MkMessage { src  = initMsg ^. #dest
                       , dest = initMsg ^. #src
                       , body = MkBody { inReplyTo = initMsg ^. #body . #msgId
                                       , msgId = Nothing
                                       , payload = InitOk
                                       }
                       }
        return nodeId
      _ -> liftIO $ fail "Not init msg"

receive :: MonadIO m => m Message
receive = do
  input <- liftIO BS.getLine
  case eitherDecodeStrict @Message input of
    Left errorMsg -> liftIO $ fail errorMsg
    Right msg -> return msg

respond :: (MonadReader env m, HasMessages env, MonadIO m) => Message -> m ()
respond msg = case msg ^. #body . #payload of
  Init _ _       -> send MkMessage { body = MkBody { payload   = InitOk, ..}, ..}
  Echo echo      -> send MkMessage { body = MkBody { payload   = EchoOk echo, ..}, ..}
  Generate       -> send MkMessage { body = MkBody { payload = GenerateOk uid, ..}, ..}
  Broadcast msg -> do
    env <- ask
    let msgs = getMsgs env
    liftIO $ atomicModifyIORef' msgs (\msgs' -> (msg : msgs', ()))
    send MkMessage { body = MkBody { payload = BroadcastOk, ..}}
  Read           -> do
    env <- ask
    msgs' <- liftIO . readIORef $ getMsgs env
    send MkMessage { body = MkBody { payload = ReadOk msgs', ..}, ..}
  Topology _     -> send MkMessage { body = MkBody { payload = TopologyOk, ..}, ..}
  _ -> return ()
  where
    src = msg ^. #dest
    dest = msg ^. #src
    inReplyTo = msg ^. #body . #msgId
    msgId = Nothing

    getUid :: NodeId -> Maybe MsgId -> Text
    getUid (MkNodeId num) (Just (MkMsgId txt)) = mconcat [ num
                                                          , "-"
                                                          , fromString $ show txt
                                                          ]
    getUid (MkNodeId num) _ = fromString $ show num ++ "it will work, right?"

    nid = msg ^. #dest
    mid = msg ^. #body . #msgId
    uid = getUid nid mid

send :: MonadIO m => Message -> m ()
send = liftIO . BL.putStrLn . encode