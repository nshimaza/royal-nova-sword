{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

{-
    Copyright (c) 2020 Cisco and/or its affiliates.

    This software is licensed to you under the terms of the Cisco Sample
    Code License, Version 1.1 (the "License"). You may obtain a copy of the
    License at

                https://developer.cisco.com/docs/licenses

    All use of the material herein must be in accordance with the terms of
    the License. All rights not expressly granted by the License are
    reserved. Unless required by applicable law or agreed to separately in
    writing, software distributed under the License is distributed on an "AS
    IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
    or implied.
-}

{-|
Module      : Network.RoyalNovaSword.Types
Copyright   : (c) Cisco and/or its affiliates
License     : Cisco Sample Code License (see the file LICENSE)
Maintainer  : https://github.com/nshimaza
Stability   : experimental

Type definitions for JSON objects and related types used in this package.
-}
module Network.RoyalNovaSword.Types where

import           GHC.Generics           (Generic)

import           Control.Applicative    (Alternative ((<|>)))
import           Data.Aeson             (FromJSON (parseJSON), Options (constructorTagModifier, omitNothingFields),
                                         ToJSON (toEncoding, toJSON),
                                         Value (Null, Object, String),
                                         defaultOptions, genericParseJSON,
                                         genericToJSON)
import           Data.Aeson.Casing      (aesonDrop, aesonPrefix, camelCase,
                                         trainCase)
import qualified Data.Attoparsec.Text   as AP (Parser, parseOnly, string,
                                               takeWhile)
import qualified Data.ByteString        as S (ByteString)
import           Data.Char              (isDigit)
import qualified Data.HashMap.Strict    as HM (lookup, singleton)
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Types.URI (urlEncode)



-- | Dummy type for filling "any" and "dst-any" field like [ null ].
data DummyNull = DummyNull deriving (Generic, Eq, Show)

instance ToJSON DummyNull where
    toJSON _ = Null

instance FromJSON DummyNull where
    parseJSON Null = pure DummyNull


-- | Interface type representation
data InterfaceType = GigabitEthernet | FastEthernet deriving (Generic, Eq, FromJSON, Show, ToJSON)

-- | Convert 'InterfaceType' to 'Text'.
interfaceTypeToText :: InterfaceType -> Text
interfaceTypeToText GigabitEthernet = "GigabitEthernet"
interfaceTypeToText FastEthernet    = "FastEthernet"

-- | Convert 'InterfaceType' to strict 'S.ByteString'.
interfaceTypeToByteString :: InterfaceType -> S.ByteString
interfaceTypeToByteString GigabitEthernet = "GigabitEthernet"
interfaceTypeToByteString FastEthernet    = "FastEthernet"

-- | Transform 'InterfaceType' to two letter interface name such as "Gi".
interfaceTypeToTwoLetterText :: InterfaceType -> Text
interfaceTypeToTwoLetterText GigabitEthernet = "Gi"
interfaceTypeToTwoLetterText FastEthernet    = "Fa"

-- | Identify switch interface.
data InterfaceId = InterfaceId
    { interfaceIdInterfaceType :: InterfaceType -- ^ Type of interface
    , interfaceIdIndex         :: Text          -- ^ Index of interface in the type.
    } deriving (Generic, Eq, Show)

-- | Generate string representation of 'InterfaceId' in 'Text'.
interfaceIdToText :: InterfaceId -> Text
interfaceIdToText (InterfaceId ifType index) = interfaceTypeToText ifType <> index

-- | Generate string representation of 'InterfaceId' in strict 'S.ByteString'
interfaceIdToByteString :: InterfaceId -> S.ByteString
interfaceIdToByteString = encodeUtf8 . interfaceIdToText

-- | URL encode string representation of 'InterfaceId' in strict 'S.ByteString'
interfaceIdToByteStringUrl :: InterfaceId -> S.ByteString
interfaceIdToByteStringUrl = urlEncode False . encodeUtf8 . interfaceIdToText

-- | Convert 'InterfaceId' to URL path of RESTCONF.
interfaceIdToRestConfPath :: InterfaceId -> S.ByteString
interfaceIdToRestConfPath (InterfaceId ifType index) = interfaceTypeToByteString ifType <> "=" <> (urlEncode False . encodeUtf8) index

-- | Parse string representation of full length interface name to 'InterfaceId'.
parseInterfaceId :: AP.Parser InterfaceId
parseInterfaceId = do
    ifType <- AP.string "GigabitEthernet" *> pure GigabitEthernet
                <|> AP.string "FastEthernet" *> pure FastEthernet
    index <- AP.takeWhile (\w -> isDigit w || w == '/')
    pure $ InterfaceId ifType index

instance ToJSON InterfaceId  where
    toJSON = String . interfaceIdToText
    toEncoding = toEncoding . interfaceIdToText

instance FromJSON InterfaceId where
    parseJSON (String t) = do
        case AP.parseOnly parseInterfaceId t of
            Left e     -> fail e
            Right ifId -> pure ifId
    parseJSON _ = fail "String is expected but got other type value."

-- | Wrapper type to parse and generate two letter form of interface name.
data TwoLetterIfId = TwoLetterIfId InterfaceId deriving (Generic, Eq, Show)

-- | Convert 'TwoLetterIfId' to two letter form of interface name in 'Text'
twoLetterIfIdToText :: TwoLetterIfId -> Text
twoLetterIfIdToText (TwoLetterIfId (InterfaceId ifType index)) = interfaceTypeToTwoLetterText ifType <> index

-- | Parse two letter form of interface name to 'TwoLetterIfId'
parseTwoLetterIfId :: AP.Parser TwoLetterIfId
parseTwoLetterIfId = do
    ifType <- AP.string "Gi" *> pure GigabitEthernet
                <|> AP.string "Fa" *> pure FastEthernet
    index <- AP.takeWhile (\w -> isDigit w || w == '/')
    pure . TwoLetterIfId $ InterfaceId ifType index

instance ToJSON TwoLetterIfId where
    toJSON = String . twoLetterIfIdToText
    toEncoding = toEncoding . twoLetterIfIdToText

instance FromJSON TwoLetterIfId where
    parseJSON (String t) = do
        case AP.parseOnly parseTwoLetterIfId t of
            Left e     -> fail e
            Right ifId -> pure ifId
    parseJSON _ = fail "String is expected but got other type value."

{-
    Types for RESTCONF JSONs
-}
-- | action field of ace-rule
data AceRuleAction = Permit | Deny deriving (Eq, Generic, Show)

instance ToJSON AceRuleAction where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = camelCase }

instance FromJSON AceRuleAction where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelCase }

-- | protocol field of ace-rule
data AceRuleProtocol = Ip | Ipv6 deriving (Eq, Generic, Show)

instance ToJSON AceRuleProtocol where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = camelCase }

instance FromJSON AceRuleProtocol where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelCase }

-- | ace-rule JSON object
data AceRule = AceRule
    { aceRuleAction   :: AceRuleAction      -- ^ Action to take on matched traffic
    , aceRuleProtocol :: AceRuleProtocol    -- ^ Protocol to match
    , aceRuleHost     :: Maybe Text         -- ^ Source IP address when match single IP address
    , aceRuleDstHost  :: Maybe Text         -- ^ Destination IP address when match single IP address
    , aceRuleAny      :: Maybe [DummyNull]  -- ^ Just [DummyNull] when match any source address
    , aceRuleDstAny   :: Maybe [DummyNull]  -- ^ Just [DummyNull] when match any destination address
    } deriving (Generic, Eq, Show)

instance ToJSON AceRule where
    toJSON = genericToJSON $ (aesonDrop 7 trainCase) { omitNothingFields = True }

instance FromJSON AceRule where
    parseJSON = genericParseJSON $ (aesonDrop 7 trainCase) { omitNothingFields = True }

-- | Element type of access-list-seq-rule list
data AclSeqRule = AclSeqRule
    { aclSeqRuleSequence :: String  -- ^ Sequence number represented in string
    , aclSeqRuleAceRule  :: AceRule -- ^ ace-rule object for the sequence number
    } deriving (Generic, Eq, Show)

instance ToJSON AclSeqRule where
    toJSON = genericToJSON $ aesonDrop 10 trainCase

instance FromJSON AclSeqRule where
    parseJSON = genericParseJSON $ aesonDrop 10 trainCase

-- | Inner JSON object of extended access list configuration
data ExtendedAclConf = ExtendedAclConf
    { extendedAclConfName              :: Text              -- ^ Name of extended access list configuration
    , extendedAclConfAccessListSeqRule :: [ AclSeqRule ]    -- ^ ace-rule list with sequence number
    } deriving (Generic, Eq, Show)

instance ToJSON ExtendedAclConf where
    toJSON = genericToJSON $ aesonDrop 15 trainCase

instance FromJSON ExtendedAclConf where
    parseJSON = genericParseJSON $ aesonDrop 15 trainCase

-- | Outer JSON object of extended access list configuration
newtype ExtendedAclConfBody = ExtendedAclConfBody ExtendedAclConf deriving (Generic, Eq, Show)

-- | Field name designating inner object of extended access list configuration
extendedAclConfLabel :: Text
extendedAclConfLabel = "Cisco-IOS-XE-acl:extended"

instance ToJSON ExtendedAclConfBody where
    toJSON (ExtendedAclConfBody conf) = Object $ HM.singleton extendedAclConfLabel $ toJSON conf

instance FromJSON ExtendedAclConfBody where
    parseJSON (Object v) = do
        case HM.lookup extendedAclConfLabel v of
            Nothing     -> fail "No \"Cisco-IOS-XE-acl:extended\" field found."
            Just inner  -> ExtendedAclConfBody <$> parseJSON inner
    parseJSON _ = fail "JSON Object is expected but got other type value."

-- | Inner JSON object for applying access list to interface
data AclIfConf = AclIfConf
    { aclIfConfAclName :: Text              -- ^ Name of extended access list to apply
    , aclIfConfIn      :: Maybe [DummyNull] -- Just [DummyNull] to apply to incoming traffic
    } deriving (Generic, Eq, Show)

instance ToJSON AclIfConf where
    toJSON = genericToJSON $ aesonDrop 9 trainCase

instance FromJSON AclIfConf where
    parseJSON = genericParseJSON $ aesonDrop 9 trainCase

-- | Outer JSON object for applying access list to interface
newtype AclIfConfBody = AclIfConfBody AclIfConf deriving (Generic, Eq, Show)

-- | Field name designating inner object for applying access list to interface
aclIfConfLabel :: Text
aclIfConfLabel = "Cisco-IOS-XE-native:acl"

instance ToJSON AclIfConfBody where
    toJSON (AclIfConfBody aclIf) = Object $ HM.singleton aclIfConfLabel $ toJSON aclIf

instance FromJSON AclIfConfBody where
    parseJSON (Object v) = do
        case HM.lookup aclIfConfLabel v of
            Nothing    -> fail "No \"Cisco-IOS-XE-native:acl\" field found."
            Just inner -> AclIfConfBody <$> parseJSON inner
    parseJSON _ = fail "JSON Object is expected but got other type value."

-- | Flow record cache entry JSON object
data Flow = Flow
    { flowSourceAddress      :: Text            -- ^ Source IP address of the flow
    , flowDestinationAddress :: Text            -- ^ Destination IP address of the flow
    , flowInterfaceInput     :: TwoLetterIfId   -- ^ Two letter interface identifier where the flow captured
    } deriving (Generic, Eq, Show)

instance ToJSON Flow where
    toJSON = genericToJSON $ aesonPrefix trainCase

instance FromJSON Flow where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

-- | JSON object to wrap list of flow monitor cache entries
data Flows = Flows { flowsFlow :: [ Flow ] } deriving (Generic, Eq, Show)

instance ToJSON Flows where
    toJSON = genericToJSON $ aesonPrefix trainCase

instance FromJSON Flows where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

-- | Inner flow monitor cache JSON object
data Cache = Cache
    { cacheName  :: Text    -- ^ Name of cached NetFlow monitor
    , cacheFlows :: Flows   -- ^ Wrapped list of flow cache entries
    } deriving (Generic, Eq, Show)

instance ToJSON Cache where
    toJSON = genericToJSON $ aesonPrefix trainCase

instance FromJSON Cache where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

-- | Outer flow record cache JSON object
newtype Monitor = Monitor Cache deriving (Generic, Eq, Show)

-- | Field name designating inner object of flow record cache
flowMonitorOperJsonFieldLabel :: Text
flowMonitorOperJsonFieldLabel = "Cisco-IOS-XE-flow-monitor-oper:flow-monitor"

instance ToJSON Monitor where
    toJSON (Monitor cache) = Object $ HM.singleton flowMonitorOperJsonFieldLabel $ toJSON cache

instance FromJSON Monitor where
    parseJSON (Object v) = do
        case HM.lookup flowMonitorOperJsonFieldLabel v of
            Nothing     -> fail "No \"Cisco-IOS-XE-flow-monitor-oper:flow-monitor\" field found."
            Just inner  -> Monitor <$> parseJSON inner
    parseJSON _ = fail "JSON Object is expected but got other type value."
