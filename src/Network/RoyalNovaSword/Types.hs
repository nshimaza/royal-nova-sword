{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

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

{-
    Interface representation
-}
data InterfaceType = GigabitEthernet | FastEthernet deriving (Generic, Eq, FromJSON, Show, ToJSON)

interfaceTypeToText :: InterfaceType -> Text
interfaceTypeToText GigabitEthernet = "GigabitEthernet"
interfaceTypeToText FastEthernet    = "FastEthernet"

interfaceTypeToByteString :: InterfaceType -> S.ByteString
interfaceTypeToByteString GigabitEthernet = "GigabitEthernet"
interfaceTypeToByteString FastEthernet    = "FastEthernet"

interfaceTypeToTwoLetterText :: InterfaceType -> Text
interfaceTypeToTwoLetterText GigabitEthernet = "Gi"
interfaceTypeToTwoLetterText FastEthernet    = "Fa"

data InterfaceId = InterfaceId
    { interfaceIdInterfaceType :: InterfaceType
    , interfaceIdIndex         :: Text
    } deriving (Generic, Eq, Show)

interfaceIdToText :: InterfaceId -> Text
interfaceIdToText (InterfaceId ifType index) = interfaceTypeToText ifType <> index

interfaceIdToByteString :: InterfaceId -> S.ByteString
interfaceIdToByteString = encodeUtf8 . interfaceIdToText

interfaceIdToByteStringUrl :: InterfaceId -> S.ByteString
interfaceIdToByteStringUrl = urlEncode False . encodeUtf8 . interfaceIdToText

interfaceIdToRestConfPath :: InterfaceId -> S.ByteString
interfaceIdToRestConfPath (InterfaceId ifType index) = interfaceTypeToByteString ifType <> "=" <> (urlEncode False . encodeUtf8) index

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

data TwoLetterIfId = TwoLetterIfId InterfaceId deriving (Generic, Eq, Show)

twoLetterIfIdToText :: TwoLetterIfId -> Text
twoLetterIfIdToText (TwoLetterIfId (InterfaceId ifType index)) = interfaceTypeToTwoLetterText ifType <> index

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
data AceRuleAction = Permit | Deny deriving (Eq, Generic, Show)

instance ToJSON AceRuleAction where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = camelCase }

instance FromJSON AceRuleAction where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelCase }

data AceRuleProtocol = Ip | Ipv6 deriving (Eq, Generic, Show)

instance ToJSON AceRuleProtocol where
    toJSON = genericToJSON defaultOptions { constructorTagModifier = camelCase }

instance FromJSON AceRuleProtocol where
    parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelCase }

data AceRule = AceRule
    { aceRuleAction   :: AceRuleAction
    , aceRuleProtocol :: AceRuleProtocol
    , aceRuleHost     :: Maybe Text
    , aceRuleDstHost  :: Maybe Text
    , aceRuleAny      :: Maybe [DummyNull]
    , aceRuleDstAny   :: Maybe [DummyNull]
    } deriving (Generic, Eq, Show)

instance ToJSON AceRule where
    toJSON = genericToJSON $ (aesonDrop 7 trainCase) { omitNothingFields = True }

instance FromJSON AceRule where
    parseJSON = genericParseJSON $ (aesonDrop 7 trainCase) { omitNothingFields = True }

data AclSeqRule = AclSeqRule
    { aclSeqRuleSequence :: String
    , aclSeqRuleAceRule  :: AceRule
    } deriving (Generic, Eq, Show)

instance ToJSON AclSeqRule where
    toJSON = genericToJSON $ aesonDrop 10 trainCase

instance FromJSON AclSeqRule where
    parseJSON = genericParseJSON $ aesonDrop 10 trainCase

data ExtendedAclConf = ExtendedAclConf
    { extendedAclConfName              :: Text
    , extendedAclConfAccessListSeqRule :: [ AclSeqRule ]
    } deriving (Generic, Eq, Show)

instance ToJSON ExtendedAclConf where
    toJSON = genericToJSON $ aesonDrop 15 trainCase

instance FromJSON ExtendedAclConf where
    parseJSON = genericParseJSON $ aesonDrop 15 trainCase

newtype ExtendedAclConfBody = ExtendedAclConfBody ExtendedAclConf deriving (Generic, Eq, Show)

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

data AclIfConf = AclIfConf
    { aclIfConfAclName :: Text
    , aclIfConfIn      :: Maybe [DummyNull]
    } deriving (Generic, Eq, Show)

instance ToJSON AclIfConf where
    toJSON = genericToJSON $ aesonDrop 9 trainCase

instance FromJSON AclIfConf where
    parseJSON = genericParseJSON $ aesonDrop 9 trainCase

newtype AclIfConfBody = AclIfConfBody AclIfConf deriving (Generic, Eq, Show)

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

data Flow = Flow
    { flowSourceAddress      :: Text
    , flowDestinationAddress :: Text
    , flowInterfaceInput     :: TwoLetterIfId
    } deriving (Generic, Eq, Show)

instance ToJSON Flow where
    toJSON = genericToJSON $ aesonPrefix trainCase

instance FromJSON Flow where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

data Flows = Flows { flowsFlow :: [ Flow ] } deriving (Generic, Eq, Show)

instance ToJSON Flows where
    toJSON = genericToJSON $ aesonPrefix trainCase

instance FromJSON Flows where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

data Cache = Cache
    { cacheName  :: Text
    , cacheFlows :: Flows
    } deriving (Generic, Eq, Show)

instance ToJSON Cache where
    toJSON = genericToJSON $ aesonPrefix trainCase

instance FromJSON Cache where
    parseJSON = genericParseJSON $ aesonPrefix trainCase

newtype Monitor = Monitor Cache deriving (Generic, Eq, Show)

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
