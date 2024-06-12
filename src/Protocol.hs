{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Protocol where

import Data.Aeson
import Data.Text (Text)
import Data.Generics.Labels ()
import GHC.Generics (Generic)

newtype NodeId = MkNodeId Text deriving (Show, Generic, ToJSON, FromJSON)
newtype MsgId  = MkMsgId Word deriving (Show, Generic, ToJSON, FromJSON)

data Message = MkMessage
  { src  :: NodeId
  , dest :: NodeId
  , body :: Body
  } deriving (Generic, Show)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \obj -> do
    src  <- obj .: "src"
    dest <- obj .: "dest"
    body <- obj .: "body"
    return MkMessage{..}
instance ToJSON Message where
  toJSON MkMessage{..} = object [ "src" .= src, "dest" .= dest, "body" .= body ]

data Body = MkBody
  { msgId     :: Maybe MsgId
  , inReplyTo :: Maybe MsgId
  , payload   :: Payload  
  } deriving (Generic, Show)

instance FromJSON Body where
  parseJSON val = withObject "Body" (\obj -> parseJSON @Payload val >>= \payload -> do
    msgId <- obj .:? "msg_id"
    inReplyTo <- obj .:? "in_reply_to"
    return MkBody {..}) val

instance ToJSON Body where
  toJSON MkBody {..} = case payload of
    Init{..} -> object $ [ "type"     .= ("init" :: Text)
                         , "node_id"  .= nodeId
                         , "node_ids" .= nodeIds
                         ] <> maybe mempty (\a -> [ "msg_id" .= a ]) msgId
    InitOk -> object $ [ "type" .= ("init_ok" :: Text)
                       ] <> maybe mempty (\a -> [ "in_reply_to" .= a ]) inReplyTo
    Echo{..} -> object $ [ "type" .= ("echo" :: Text) 
                         , "echo" .= echo
                         ]
                        <> maybe mempty (\a -> [ "msg_id" .= a ]) msgId
    EchoOk{..} -> object $ [ "type" .= ("echo_ok" :: Text)
                           , "echo" .= echo 
                           ] 
                          <> maybe mempty (\a -> pure $ "msg_id" .= a) msgId
                          <> maybe mempty (\a -> pure $ "in_reply_to" .= a) inReplyTo

data Payload = Init   { nodeId :: NodeId, nodeIds :: [NodeId] }
             | InitOk
             | Echo   { echo :: Text }
             | EchoOk { echo :: Text }
  deriving (Generic, Show)

instance FromJSON Payload where
  parseJSON = genericParseJSON opts

instance ToJSON Payload where
  toJSON = genericToJSON opts

opts :: Options
opts = defaultOptions { fieldLabelModifier = camelTo2 '_'
                      , constructorTagModifier = camelTo2 '_'
                      , omitNothingFields = True
                      , sumEncoding = defaultTaggedObject { tagFieldName = "type" }
                      }