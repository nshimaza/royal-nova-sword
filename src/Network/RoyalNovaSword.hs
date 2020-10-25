{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Network.RoyalNovaSword where

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
data RestConfAccessProfile = RestConfAccessProfile
    { restConfAccessProfileHost              :: S.ByteString
    , restConfAccessProfileUsername          :: S.ByteString
    , restConfAccessProfilePassword          :: S.ByteString
    , restConfAccessProfileTLSValidateServer :: Bool
    }

{-
    Flow to ACL converter
-}
flowToAceRule :: Flow -> AceRule
flowToAceRule (Flow src dst _) = AceRule Permit Ip (Just src) (Just dst) Nothing Nothing

flowsForOneIfToAclSeqRules :: [Flow] -> [AclSeqRule]
flowsForOneIfToAclSeqRules = map (\(i, flow) -> AclSeqRule (show i) (flowToAceRule flow)) . zip [1..]

appendDenyAll :: [AclSeqRule] -> [AclSeqRule]
appendDenyAll rules = rules <> [denyAll]
  where
    denyAll = AclSeqRule (show (1 + length rules)) $ AceRule Deny Ip Nothing Nothing (Just [DummyNull]) (Just [DummyNull])

makeExtAcl :: [Flow] -> InterfaceId -> ExtendedAclConfBody
makeExtAcl flows ifId = makeBody $ flowsForOneIfToAclSeqRules targets
  where
    targets = filter (\(Flow _ _ (TwoLetterIfId i)) -> i == ifId) flows
    makeBody = ExtendedAclConfBody . ExtendedAclConf (interfaceIdToText ifId) . appendDenyAll

makeExtAcls :: [Flow] -> [InterfaceId] -> [ExtendedAclConfBody]
makeExtAcls flows ifIds = makeExtAcl flows <$> ifIds


{-
    HTTP connection configuration
-}
createManager :: MonadIO m => Bool -> m Manager
createManager validate = do
    let settings = mkManagerSettings (TLSSettingsSimple (not validate) False False) Nothing
    liftIO $ newManager settings

{-
    RESTCONF paths
-}
restConfCommonPath :: S.ByteString
restConfCommonPath = "/restconf/data"

restConfNativeConfPath :: S.ByteString
restConfNativeConfPath = restConfCommonPath <> "/Cisco-IOS-XE-native:native"

restConfNetFlowConfPath :: S.ByteString
restConfNetFlowConfPath = restConfNativeConfPath <> "/flow"

restConfInterfaceConfPath :: InterfaceId -> S.ByteString
restConfInterfaceConfPath ifId = restConfNativeConfPath <> "/interface/" <> interfaceIdToRestConfPath ifId

restConfInterfaceFlowConfPath :: InterfaceId -> S.ByteString
restConfInterfaceFlowConfPath ifId = restConfInterfaceConfPath ifId <> "/ip/flow"

restConfInterfaceAclInConfPath :: InterfaceId -> S.ByteString
restConfInterfaceAclInConfPath ifId = restConfInterfaceConfPath ifId <> "/ip/access-group/in/acl"

restConfExtAclConfPath :: Text -> S.ByteString
restConfExtAclConfPath name = restConfNativeConfPath <> "/ip/access-list/extended=" <> (urlEncode False $ encodeUtf8 name)

restConfAllAclConfPath :: S.ByteString
restConfAllAclConfPath = restConfNativeConfPath <> "/ip/access-list"

restConfFlowMonitorOprPath :: S.ByteString
restConfFlowMonitorOprPath = restConfCommonPath <> "/Cisco-IOS-XE-flow-monitor-oper:flow-monitors/flow-monitor=IPFlow"

addRestConfJsonHeaders :: Request -> Request
addRestConfJsonHeaders req
    = addRequestHeader hAccept "application/yang-data+json"
    $ addRequestHeader hContentType "application/yang-data+json"
    $ req

{-
    NetFlow configuration
-}
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

deleteNetFlowMonOnInterfaces :: MonadIO m => RestConfAccessProfile -> [InterfaceId] -> m (Either (Response ()) ())
deleteNetFlowMonOnInterfaces accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- deleteNetFlowMonOnInterface accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

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

setAclInOnInterfaces :: MonadIO f => RestConfAccessProfile -> [InterfaceId] -> f (Either (Response ()) ())
setAclInOnInterfaces accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- setAclInOnInterface accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

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

deleteAclInOnInterfaces :: MonadIO f => RestConfAccessProfile -> [InterfaceId] -> f (Either (Response ()) ())
deleteAclInOnInterfaces accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- deleteAclInOnInterface accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

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

setExtAcls :: MonadIO f => RestConfAccessProfile -> [ExtendedAclConfBody] -> f (Either (Response ()) ())
setExtAcls accessProfile acls = go acls
  where
    go []           = pure $ Right ()
    go (acl:rest)   = do
        result <- setExtAcl accessProfile acl
        case result of
            Right () -> go rest
            err      -> pure err

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

deleteExtAcls :: MonadIO f => RestConfAccessProfile -> [InterfaceId] -> f (Either (Response ()) ())
deleteExtAcls accessProfile ifIds = go ifIds
  where
    go []           = pure $ Right ()
    go (ifId:rest)  = do
        result <- deleteExtAcl accessProfile ifId
        case result of
            Right () -> go rest
            err      -> pure err

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
