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
Module      : Network.RoyalNovaSword
Copyright   : (c) Cisco and/or its affiliates
License     : Cisco Sample Code License (see the file LICENSE)
Maintainer  : https://github.com/nshimaza
Stability   : experimental

Creating Flexible NetFlow monitor on switch, obtain learned flows, applying
access control allowing only flows presented during learning period.
-}
module Network.RoyalNovaSword
    (
    -- * Types
      RestConfAccessProfile(..)
    -- * Flow to ACL converting functions
    , flowToAceRule
    , flowsForOneIfToAclSeqRules
    , appendDenyAll
    , makeExtAcl
    , makeExtAcls
    -- * HTTP client configuration
    , createManager
    -- * RESTCONF constants
    , restConfCommonPath
    , restConfNativeConfPath
    , restConfNetFlowConfPath
    , restConfInterfaceConfPath
    , restConfInterfaceFlowConfPath
    , restConfInterfaceAclInConfPath
    , restConfExtAclConfPath
    , restConfAllAclConfPath
    , restConfFlowMonitorOprPath
    , addRestConfJsonHeaders
    -- * RESTCONF operations
    -- ** Flexible NetFlow operations
    , deleteNetFlowConf
    , netFlowRestConfBody
    , setNetFlowConf
    , deleteNetFlowMonOnInterface
    , deleteNetFlowMonOnInterfaces
    , netFlowInterfaceRestConfBody
    , setNetFlowMonOnInterface
    , setNetFlowMonOnInterfaces
    -- ** Access Control List operations
    , setAclInOnInterface
    , setAclInOnInterfaces
    , deleteAclInOnInterface
    , deleteAclInOnInterfaces
    , setExtAcl
    , setExtAcls
    , deleteExtAcl
    , deleteExtAcls
    , deleteAllAcls
    , getFlowMonitorCache
    ) where

import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Aeson                   (encode)
import qualified Data.ByteString              as S (ByteString)
import qualified Data.ByteString.Lazy         as L (ByteString)
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)
import           Network.Connection           (TLSSettings (TLSSettingsSimple))
import           Network.HTTP.Conduit         (Manager, Request, Response,
                                               defaultRequest,
                                               mkManagerSettings, newManager)
import           Network.HTTP.Simple          (addRequestHeader,
                                               getResponseBody,
                                               getResponseStatus, httpJSON,
                                               httpNoBody, setRequestBasicAuth,
                                               setRequestBodyLBS,
                                               setRequestHost,
                                               setRequestManager,
                                               setRequestMethod, setRequestPath,
                                               setRequestPort, setRequestSecure)
import           Network.HTTP.Types.Header    (hAccept, hContentType)
import           Network.HTTP.Types.Status    (created201, noContent204,
                                               notFound404, ok200)
import           Network.HTTP.Types.URI       (urlEncode)

import           Network.RoyalNovaSword.Types


{-
    Types
-}
-- | Access profile for RESTCONF on switch
data RestConfAccessProfile = RestConfAccessProfile
    { restConfAccessProfileHost              :: S.ByteString    -- ^ Target switch host name or IP address
    , restConfAccessProfileUsername          :: S.ByteString    -- ^ Username for accessing RESTCONF of the switch
    , restConfAccessProfilePassword          :: S.ByteString    -- ^ Password for accessing RESTCONF of the switch
    , restConfAccessProfileTLSValidateServer :: Bool            -- ^ False when the switch certificate is self-signed
    }

{-
    Flow to ACL converter
-}
-- | Convert single flow entry to ace-rule entry of access control list
flowToAceRule :: Flow -> AceRule
flowToAceRule (Flow src dst _) = AceRule Permit Ip (Just src) (Just dst) Nothing Nothing

-- | Convert list of flows at single interface to list of ace-rule with auto-generated sequence number
flowsForOneIfToAclSeqRules :: [Flow] -> [AclSeqRule]
flowsForOneIfToAclSeqRules = map (\(i, flow) -> AclSeqRule (show i) (flowToAceRule flow)) . zip [1..]

-- | Append deny-all rule at the end of sequenced ace-rule list
appendDenyAll :: [AclSeqRule] -> [AclSeqRule]
appendDenyAll rules = rules <> [denyAll]
  where
    denyAll = AclSeqRule (show (1 + length rules)) $ AceRule Deny Ip Nothing Nothing (Just [DummyNull]) (Just [DummyNull])

-- | Filter flows by given 'InterfaceId' and convert it to extended access list configuration
makeExtAcl :: [Flow] -> InterfaceId -> ExtendedAclConfBody
makeExtAcl flows ifId = makeBody $ flowsForOneIfToAclSeqRules targets
  where
    targets = filter (\(Flow _ _ (TwoLetterIfId i)) -> i == ifId) flows
    makeBody = ExtendedAclConfBody . ExtendedAclConf (interfaceIdToText ifId) . appendDenyAll

-- | Create list of extended ACL configuration from given list of flows and list of interfaces
makeExtAcls :: [Flow] -> [InterfaceId] -> [ExtendedAclConfBody]
makeExtAcls flows ifIds = makeExtAcl flows <$> ifIds


{-
    HTTP connection configuration
-}
-- | Set up TLS parameters for RESTCONF HTTPS access.
createManager :: MonadIO m => Bool -> m Manager
createManager validate = do
    let settings = mkManagerSettings (TLSSettingsSimple (not validate) False False) Nothing
    liftIO $ newManager settings

{-
    RESTCONF paths
-}
-- | Most common part of URL path for RESTCONF
restConfCommonPath :: S.ByteString
restConfCommonPath = "/restconf/data"

-- | RESTCONF URL path for Cisco IOS XE native configuration
restConfNativeConfPath :: S.ByteString
restConfNativeConfPath = restConfCommonPath <> "/Cisco-IOS-XE-native:native"

-- | RESTCONF URL path for IOS XE Flexible NetFlow configuration
restConfNetFlowConfPath :: S.ByteString
restConfNetFlowConfPath = restConfNativeConfPath <> "/flow"

-- | RESTCONF path to designate interface for configuration
restConfInterfaceConfPath :: InterfaceId -> S.ByteString
restConfInterfaceConfPath ifId = restConfNativeConfPath <> "/interface/" <> interfaceIdToRestConfPath ifId

-- | RESTCONF path to apply NetFlow monitor to specified interface
restConfInterfaceFlowConfPath :: InterfaceId -> S.ByteString
restConfInterfaceFlowConfPath ifId = restConfInterfaceConfPath ifId <> "/ip/flow"

-- | RESTCONF path to apply input access control list to specified interface
restConfInterfaceAclInConfPath :: InterfaceId -> S.ByteString
restConfInterfaceAclInConfPath ifId = restConfInterfaceConfPath ifId <> "/ip/access-group/in/acl"

-- | RESTCONF path to designate named extended access list
restConfExtAclConfPath :: Text -> S.ByteString
restConfExtAclConfPath name = restConfNativeConfPath <> "/ip/access-list/extended=" <> (urlEncode False $ encodeUtf8 name)

-- | RESTCONF path to designate entire access list configuration
restConfAllAclConfPath :: S.ByteString
restConfAllAclConfPath = restConfNativeConfPath <> "/ip/access-list"

-- | RESTCONF path to obtain current cache of Flexible NetFlow monitor
restConfFlowMonitorOprPath :: S.ByteString
restConfFlowMonitorOprPath = restConfCommonPath <> "/Cisco-IOS-XE-flow-monitor-oper:flow-monitors/flow-monitor=IPFlow"

-- | Common HTTP header for RESTCONF indicating this RESTCONF client only send and receive JSON format of YANG data
addRestConfJsonHeaders :: Request -> Request
addRestConfJsonHeaders req
    = addRequestHeader hAccept "application/yang-data+json"
    $ addRequestHeader hContentType "application/yang-data+json"
    $ req

{-
    NetFlow configuration
-}
-- | Delete NetFlow configuration
deleteNetFlowConf :: MonadIO m => RestConfAccessProfile -> m (Either (Response ()) ())
deleteNetFlowConf (RestConfAccessProfile host user pass validate) = do
    manager <- createManager validate
    let req = setRequestMethod "DELETE"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath restConfNetFlowConfPath
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == noContent204
            then Right ()
            else Left res

-- | RESTCONF body JSON to create Flexible NetFlow record and monitor configuration
netFlowRestConfBody :: L.ByteString
netFlowRestConfBody =
    "{\
    \  \"Cisco-IOS-XE-native:flow\": {\
    \    \"Cisco-IOS-XE-flow:record\": [\
    \      {\
    \        \"name\": \"IPFlow\",\
    \        \"match\": {\
    \          \"interface\": { \"input\": {} },\
    \          \"ipv4\": {\
    \            \"destination\": { \"address\": [ null ] },\
    \            \"source\": { \"address\": [ null ] }\
    \          }\
    \        }\
    \      }\
    \    ],\
    \    \"Cisco-IOS-XE-flow:monitor\": [\
    \      {\
    \        \"name\": \"IPFlow\",\
    \        \"cache\": {\
    \          \"timeout\": {\
    \            \"active\": 604800,\
    \            \"inactive\": 604800\
    \          }\
    \        },\
    \        \"record\": { \"type\": \"IPFlow\" }\
    \      }\
    \    ]\
    \  }\
    \}"

-- | Create Flexible NetFlow custom record and monitor configuration
setNetFlowConf :: MonadIO m => RestConfAccessProfile -> m (Either (Response ()) ())
setNetFlowConf (RestConfAccessProfile host user pass validate) = do
    manager <- createManager validate
    let req = setRequestMethod "PUT"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestBodyLBS netFlowRestConfBody
            $ setRequestPath restConfNetFlowConfPath
            $ addRestConfJsonHeaders
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == noContent204
            then Right ()
            else Left res

-- | Remove NetFlow monitor from given interface
deleteNetFlowMonOnInterface :: MonadIO m => RestConfAccessProfile -> InterfaceId -> m (Either (Response ()) ())
deleteNetFlowMonOnInterface (RestConfAccessProfile host user pass validate) ifId = do
    manager <- createManager validate
    let req = setRequestMethod "DELETE"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath (restConfInterfaceFlowConfPath ifId)
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == noContent204
            then Right ()
            else Left res

-- | Remove NetFlow monitor from given interface list
deleteNetFlowMonOnInterfaces :: MonadIO m => RestConfAccessProfile -> [InterfaceId] -> m (Either (Response ()) ())
deleteNetFlowMonOnInterfaces accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- deleteNetFlowMonOnInterface accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

-- | RESTCONF body JSON to apply Flexible NetFlow monitor to an interface
netFlowInterfaceRestConfBody :: L.ByteString
netFlowInterfaceRestConfBody =
    "{\
    \  \"Cisco-IOS-XE-flow:flow\": {\
    \    \"monitor-new\": [\
    \      {\
    \        \"name\": \"IPFlow\",\
    \        \"direction\": \"input\"\
    \      }\
    \    ]\
    \  }\
    \}"

-- | Apply NetFlow monitor to given interface
setNetFlowMonOnInterface :: MonadIO m => RestConfAccessProfile -> InterfaceId -> m (Either (Response ()) ())
setNetFlowMonOnInterface (RestConfAccessProfile host user pass validate) ifId = do
    manager <- createManager validate
    let req = setRequestMethod "PUT"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestBodyLBS netFlowInterfaceRestConfBody
            $ setRequestPath (restConfInterfaceFlowConfPath ifId)
            $ addRestConfJsonHeaders
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == noContent204
            then Right ()
            else Left res

-- | Apply NetFlow monitor to given interface list
setNetFlowMonOnInterfaces :: MonadIO m => RestConfAccessProfile -> [InterfaceId] -> m (Either (Response ()) ())
setNetFlowMonOnInterfaces accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- setNetFlowMonOnInterface accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

{-
    ACL configuration
-}
-- | Apply input access list to given interface
setAclInOnInterface :: MonadIO m => RestConfAccessProfile -> InterfaceId -> m (Either (Response ()) ())
setAclInOnInterface (RestConfAccessProfile host user pass validate) ifId = do
    manager <- createManager validate
    let req = setRequestMethod "PUT"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath (restConfInterfaceAclInConfPath ifId)
            $ setRequestBodyLBS (encode $ AclIfConfBody $ AclIfConf (interfaceIdToText ifId) (Just [DummyNull]))
            $ addRestConfJsonHeaders
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == noContent204
            then Right ()
            else Left res

-- | Apply input access lists to given interface list
setAclInOnInterfaces :: MonadIO f => RestConfAccessProfile -> [InterfaceId] -> f (Either (Response ()) ())
setAclInOnInterfaces accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- setAclInOnInterface accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

-- | Remove input access list from given interface
deleteAclInOnInterface :: MonadIO m => RestConfAccessProfile -> InterfaceId -> m (Either (Response ()) ())
deleteAclInOnInterface (RestConfAccessProfile host user pass validate) ifId = do
    manager <- createManager validate
    let req = setRequestMethod "DELETE"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath (restConfInterfaceAclInConfPath ifId)
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == noContent204
            then Right ()
            else Left res

-- | Remove input access lists from given interface list
deleteAclInOnInterfaces :: MonadIO f => RestConfAccessProfile -> [InterfaceId] -> f (Either (Response ()) ())
deleteAclInOnInterfaces accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- deleteAclInOnInterface accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

-- | Create an extended access list on target switch
setExtAcl :: MonadIO m => RestConfAccessProfile -> ExtendedAclConfBody -> m (Either (Response ()) ())
setExtAcl (RestConfAccessProfile host user pass validate) acl@(ExtendedAclConfBody (ExtendedAclConf name _)) = do
    manager <- createManager validate
    let req = setRequestMethod "PUT"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath (restConfExtAclConfPath name)
            $ setRequestBodyLBS (encode acl)
            $ addRestConfJsonHeaders
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == created201
            then Right ()
            else Left res

-- | Create all given extended access lists on target switch
setExtAcls :: MonadIO f => RestConfAccessProfile -> [ExtendedAclConfBody] -> f (Either (Response ()) ())
setExtAcls accessProfile acls = go acls
  where
    go []           = pure $ Right ()
    go (acl:rest)   = do
        result <- setExtAcl accessProfile acl
        case result of
            Right () -> go rest
            err      -> pure err

-- | Delete extended access list entry for given interface
deleteExtAcl :: MonadIO m => RestConfAccessProfile -> InterfaceId -> m (Either (Response ()) ())
deleteExtAcl (RestConfAccessProfile host user pass validate) ifId = do
    manager <- createManager validate
    let req = setRequestMethod "DELETE"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath (restConfExtAclConfPath $ interfaceIdToText ifId)
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $ case getResponseStatus res of
            s | s == noContent204   -> Right ()
              | s == notFound404    -> Right ()  -- Ignore if the ACL was not defined.
              | otherwise           -> Left res

-- | Delete all extended access list entries for given interface list
deleteExtAcls :: MonadIO f => RestConfAccessProfile -> [InterfaceId] -> f (Either (Response ()) ())
deleteExtAcls accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- deleteExtAcl accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

-- | Delete entire access list configuration from the target switch
deleteAllAcls :: MonadIO m => RestConfAccessProfile -> m (Either (Response ()) ())
deleteAllAcls (RestConfAccessProfile host user pass validate) = do
    manager <- createManager validate
    let req = setRequestMethod "DELETE"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath restConfAllAclConfPath
            $ setRequestManager manager
            $ defaultRequest
    res <- httpNoBody req
    pure $  if getResponseStatus res == noContent204
            then Right ()
            else Left res

{-
    NetFlow monitor operations
-}
-- | Obtain current cache of Flexible NetFlow monitor
getFlowMonitorCache :: MonadIO m => RestConfAccessProfile -> m (Either (Response Monitor) Monitor)
getFlowMonitorCache (RestConfAccessProfile host user pass validate) = do
    manager <- createManager validate
    let req = setRequestMethod "GET"
            $ setRequestBasicAuth user pass
            $ setRequestHost host
            $ setRequestPort 443
            $ setRequestSecure True
            $ setRequestPath restConfFlowMonitorOprPath
            $ addRestConfJsonHeaders
            $ setRequestManager manager
            $ defaultRequest
    res <- httpJSON req
    pure $  if getResponseStatus res == ok200
            then Right $ getResponseBody res
            else Left res
