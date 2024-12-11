{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Protocol where

import Data.Aeson
import Data.Aeson.KeyMap (fromList, union)
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Map (Map)
import Data.Maybe (catMaybes)


newtype NodeId = MkNodeId Text deriving (Show, Eq, Ord, Generic, ToJSONKey, FromJSONKey, ToJSON, FromJSON)
newtype MsgId  = MkMsgId Word deriving (Show, Generic, ToJSON, FromJSON)
newtype BCMsg  = MkBCMsg Int deriving (Show, Generic, ToJSON, FromJSON)

data Message = MkMessage
  { src  :: NodeId
  , dest :: NodeId
  , body :: Body
  } deriving (Generic, Show)

instance FromJSON Message

instance ToJSON Message where
  toEncoding MkMessage{..} = pairs $ "src" .= src <> "dest" .= dest <> "body" .= body

data Body = MkBody
  { msgId     :: Maybe MsgId
  , inReplyTo :: Maybe MsgId
  , payload   :: Payload  
  } deriving (Generic, Show)

instance FromJSON Body where
  parseJSON v = parseJSON @Payload v >>= \p -> withObject "msgBody" (\obj -> MkBody
    <$> obj .:? "msg_id"
    <*> obj .:? "in_reply_to"
    <*> pure p) v

instance ToJSON Body where
  toJSON MkBody{..} = Object $ fromList kvs `union` toKM (toJSON payload)
    where
      toKM = \case
        Object km -> km
        _         -> fromList []
      
      kvs = map (uncurry (.=)) . catMaybes $ zipWith (\k v -> case v of Nothing -> Nothing; Just v' -> Just (k, v')) ["msg_id", "in_reply_to"] [msgId, inReplyTo]

data Payload = Init       { nodeId :: NodeId, nodeIds :: [NodeId] }
             | InitOk
             | Echo       { echo :: Text }
             | EchoOk     { echo :: Text }
             | Generate
             | GenerateOk { id :: Text }
             | Broadcast  { message :: BCMsg }
             | BroadcastOk
             | Read
             | ReadOk     { messages :: [BCMsg] }
             | Topology   { topology :: Maybe (Map NodeId [NodeId]) }
             | TopologyOk
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