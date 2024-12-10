{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Protocol where

import Data.Aeson
import Data.Aeson.KeyMap (fromList, union)
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)

newtype NodeId = MkNodeId Text deriving (Show, Generic, ToJSON, FromJSON)
newtype MsgId  = MkMsgId Word deriving (Show, Generic, ToJSON, FromJSON)

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
    <$> obj .: "msg_id"
    <*> obj .:? "in_reply_to"
    <*> pure p) v

instance ToJSON Body where
  toJSON MkBody{..} = Object $ fromList kvs `union` toKM (toJSON payload)
    where
      toKM = \case
        Object km -> km
        _         -> fromList []
      
      kvs = case inReplyTo of
        Nothing  -> ["msg_id" .= msgId]
        Just irt -> ["msg_id" .= msgId, "in_reply_to" .= irt]

data Payload = Init   { nodeId :: NodeId, nodeIds :: [NodeId] }
             | InitOk
             | Echo   { echo :: Text }
             | EchoOk { echo :: Text }
             | Generate
             | GenerateOk { id :: Text }
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