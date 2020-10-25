{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Network.RoyalNovaSword.TypesSpec where

import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.ByteString.Lazy
import           Data.Foldable
import           Data.Text.Encoding

import           Test.Hspec

import           Network.RoyalNovaSword.Types

spec :: Spec
spec = do
    let aceRulePermitHost = AceRule { aceRuleAction   = Permit
                                    , aceRuleProtocol = Ip
                                    , aceRuleHost     = Just "1.2.3.4"
                                    , aceRuleDstHost  = Just "5.6.7.8"
                                    , aceRuleAny      = Nothing
                                    , aceRuleDstAny   = Nothing
                                    }
        aceRuleJsonPermitHost = "{\
                                \  \"action\": \"permit\",\
                                \  \"protocol\": \"ip\",\
                                \  \"host\": \"1.2.3.4\",\
                                \  \"dst-host\": \"5.6.7.8\"\
                                \}"
        aceRuleDenyAny = AceRule { aceRuleAction      = Deny
                                    , aceRuleProtocol = Ip
                                    , aceRuleHost     = Nothing
                                    , aceRuleDstHost  = Nothing
                                    , aceRuleAny      = Just [DummyNull]
                                    , aceRuleDstAny   = Just [DummyNull]
                                    }
        aceRuleJsonDenyAny = "{\
                             \  \"action\": \"deny\",\
                             \  \"protocol\": \"ip\",\
                             \  \"any\": [ null ],\
                             \  \"dst-any\": [ null ]\
                             \}"
        aclSeqPermitHost10 = AclSeqRule { aclSeqRuleSequence = "10"
                                        , aclSeqRuleAceRule = aceRulePermitHost
                                        }
        aclSeqJsonPermitHost10 = "{\"sequence\": \"10\", \"ace-rule\": " <> aceRuleJsonPermitHost <> "}"
        aclSeqDenyAny20 = AclSeqRule { aclSeqRuleSequence = "20"
                                     , aclSeqRuleAceRule = aceRuleDenyAny
                                     }
        aclSeqJsonDenyAny20 = "{\"sequence\": \"20\", \"ace-rule\": " <> aceRuleJsonDenyAny <> "}"


        extendedAclConf = ExtendedAclConf { extendedAclConfName = "AclName"
                                          , extendedAclConfAccessListSeqRule = [ aclSeqPermitHost10, aclSeqDenyAny20 ]
                                          }
        extendedAclConfJson = "{\"name\": \"AclName\", \"access-list-seq-rule\": ["
            <> aclSeqJsonPermitHost10 <> ", " <> aclSeqJsonDenyAny20 <> "]}"
        extendedAclConfBody = ExtendedAclConfBody extendedAclConf
        extendedAclConfBodyJson = "{\"Cisco-IOS-XE-acl:extended\": " <> extendedAclConfJson <> "}"
        aclIfConf = AclIfConf "AclName" (Just [DummyNull])
        aclIfConfJson = "{\"acl-name\": \"AclName\", \"in\": [ null ]}"
        aclIfConfBody = AclIfConfBody aclIfConf
        aclIfConfBodyJson = "{\"Cisco-IOS-XE-native:acl\": " <> aclIfConfJson <> "}"
        flow1 = Flow { flowSourceAddress      = "192.168.1.1"
                     , flowDestinationAddress = "10.71.46.44"
                     , flowInterfaceInput     = TwoLetterIfId $ InterfaceId GigabitEthernet "2/1"
                     }
        flowJson1 = "{\
                    \  \"source-address\": \"192.168.1.1\",\
                    \  \"destination-address\": \"10.71.46.44\",\
                    \  \"interface-input\": \"Gi2/1\",\
                    \  \"is-multicast\": \"\",\
                    \  \"vrf-id-input\": \"0\",\
                    \  \"source-port\": \"0\",\
                    \  \"destination-port\": \"0\",\
                    \  \"ip-tos\": \"\",\
                    \  \"ip-protocol\": \"0\",\
                    \  \"interface-output\": \"\",\
                    \  \"bytes\": \"0\",\
                    \  \"packets\": \"0\"\
                    \}"
        flow2 = Flow { flowSourceAddress      = "192.168.1.1"
                     , flowDestinationAddress = "192.168.1.2"
                     , flowInterfaceInput     = TwoLetterIfId $ InterfaceId GigabitEthernet "2/1"
                     }
        flowJson2 = "{\
                    \  \"source-address\": \"192.168.1.1\",\
                    \  \"destination-address\": \"192.168.1.2\",\
                    \  \"interface-input\": \"Gi2/1\",\
                    \  \"is-multicast\": \"\",\
                    \  \"vrf-id-input\": \"0\",\
                    \  \"source-port\": \"0\",\
                    \  \"destination-port\": \"0\",\
                    \  \"ip-tos\": \"\",\
                    \  \"ip-protocol\": \"0\",\
                    \  \"interface-output\": \"\",\
                    \  \"bytes\": \"0\",\
                    \  \"packets\": \"0\"\
                    \}"
        flow3 = Flow { flowSourceAddress      = "192.168.1.2"
                     , flowDestinationAddress = "192.168.1.1"
                     , flowInterfaceInput     = TwoLetterIfId $ InterfaceId GigabitEthernet "2/2"
                     }
        flowJson3 = "{\
                    \  \"source-address\": \"192.168.1.2\",\
                    \  \"destination-address\": \"192.168.1.1\",\
                    \  \"interface-input\": \"Gi2/2\",\
                    \  \"is-multicast\": \"\",\
                    \  \"vrf-id-input\": \"0\",\
                    \  \"source-port\": \"0\",\
                    \  \"destination-port\": \"0\",\
                    \  \"ip-tos\": \"\",\
                    \  \"ip-protocol\": \"0\",\
                    \  \"interface-output\": \"\",\
                    \  \"bytes\": \"0\",\
                    \  \"packets\": \"0\"\
                    \}"
        flow4 = Flow { flowSourceAddress      = "192.168.1.2"
                     , flowDestinationAddress = "10.71.46.44"
                     , flowInterfaceInput     = TwoLetterIfId $ InterfaceId GigabitEthernet "2/2"
                     }
        flowJson4 = "{\
                    \  \"source-address\": \"192.168.1.2\",\
                    \  \"destination-address\": \"10.71.46.44\",\
                    \  \"interface-input\": \"Gi2/2\",\
                    \  \"is-multicast\": \"\",\
                    \  \"vrf-id-input\": \"0\",\
                    \  \"source-port\": \"0\",\
                    \  \"destination-port\": \"0\",\
                    \  \"ip-tos\": \"\",\
                    \  \"ip-protocol\": \"0\",\
                    \  \"interface-output\": \"\",\
                    \  \"bytes\": \"0\",\
                    \  \"packets\": \"0\"\
                    \}"
        flows = Flows [ flow1, flow2, flow3, flow4 ]
        flowsJson = "{\"flow\":[" <> flowJson1 <> "," <> flowJson2 <> "," <> flowJson3 <> "," <> flowJson4 <> "]}"
        cache = Cache { cacheName  = "IPFlow"
                      , cacheFlows = flows
                      }
        cacheJson = "{\"name\": \"IPFlow\", \"time-collected\": \"3496446888\", \"flows\": " <> flowsJson <> "}"
        monitorJson = "{\"Cisco-IOS-XE-flow-monitor-oper:flow-monitor\": " <> cacheJson <> "}"
        monitor = Monitor cache

    describe "Full length interface name parser" $ do
        it "Interface name pattens" $ do
            let patterns = [ ("GigabitEthernet0", InterfaceId GigabitEthernet "0" )
                           , ("GigabitEthernet1", InterfaceId GigabitEthernet "1" )
                           , ("GigabitEthernet2/3", InterfaceId GigabitEthernet "2/3" )
                           , ("GigabitEthernet4/5", InterfaceId GigabitEthernet "4/5" )
                           , ("GigabitEthernet6/7/8", InterfaceId GigabitEthernet "6/7/8" )
                           , ("GigabitEthernet9/10/11", InterfaceId GigabitEthernet "9/10/11" )
                           , ("FastEthernet0", InterfaceId FastEthernet "0" )
                           , ("FastEthernet1", InterfaceId FastEthernet "1" )
                           , ("FastEthernet2/3", InterfaceId FastEthernet "2/3" )
                           , ("FastEthernet4/5", InterfaceId FastEthernet "4/5" )
                           , ("FastEthernet6/7/8", InterfaceId FastEthernet "6/7/8" )
                           , ("FastEthernet9/10/11", InterfaceId FastEthernet "9/10/11" )
                           ]
            for_ patterns $ \(src, dst) -> do
                let jsn = fromStrict . encodeUtf8 $ "\"" <> src <> "\""
                parseOnly parseInterfaceId src `shouldBe` Right dst
                decode jsn `shouldBe` Just dst
                encode dst `shouldBe` jsn
                (decode . encode) dst `shouldBe` Just dst

    describe "Two letter interface name parser" $ do
        it "Interface name pattens" $ do
            let patterns = [ ("Gi0", TwoLetterIfId (InterfaceId GigabitEthernet "0") )
                           , ("Gi1", TwoLetterIfId (InterfaceId GigabitEthernet "1") )
                           , ("Gi2/3", TwoLetterIfId (InterfaceId GigabitEthernet "2/3") )
                           , ("Gi4/5", TwoLetterIfId (InterfaceId GigabitEthernet "4/5") )
                           , ("Gi6/7/8", TwoLetterIfId (InterfaceId GigabitEthernet "6/7/8") )
                           , ("Gi9/10/11", TwoLetterIfId (InterfaceId GigabitEthernet "9/10/11") )
                           , ("Fa0", TwoLetterIfId (InterfaceId FastEthernet "0") )
                           , ("Fa1", TwoLetterIfId (InterfaceId FastEthernet "1") )
                           , ("Fa2/3", TwoLetterIfId (InterfaceId FastEthernet "2/3") )
                           , ("Fa4/5", TwoLetterIfId (InterfaceId FastEthernet "4/5") )
                           , ("Fa6/7/8", TwoLetterIfId (InterfaceId FastEthernet "6/7/8") )
                           , ("Fa9/10/11", TwoLetterIfId (InterfaceId FastEthernet "9/10/11") )
                           ]
            for_ patterns $ \(src, dst) -> do
                let jsn = fromStrict . encodeUtf8 $ "\"" <> src <> "\""
                parseOnly parseTwoLetterIfId src `shouldBe` Right dst
                decode jsn `shouldBe` Just dst
                encode dst `shouldBe` jsn
                (decode . encode) dst `shouldBe` Just dst

    describe "AceRuleAction" $ do
        it "Permit" $ do
            encode Permit `shouldBe` "\"permit\""

        it "Deny" $ do
            encode Deny `shouldBe` "\"deny\""

    describe "AceRuleProtocol" $ do
        it "Ip" $ do
            encode Ip `shouldBe` "\"ip\""

        it "Ipv6" $ do
            encode Ipv6 `shouldBe` "\"ipv6\""

    describe "AceRule" $ do
        it "decode permit host ace-rule" $ do
            decode aceRuleJsonPermitHost `shouldBe` Just aceRulePermitHost
            (decode . encode) aceRulePermitHost `shouldBe` Just aceRulePermitHost

        it "decode deny any ace-rule" $ do
            decode aceRuleJsonDenyAny `shouldBe` Just aceRuleDenyAny
            (decode . encode) aceRuleDenyAny `shouldBe` Just aceRuleDenyAny

    describe "AclSeqRule" $ do
        it "decode permit host ace-rule with sequence 10" $ do
            decode aclSeqJsonPermitHost10 `shouldBe` Just aclSeqPermitHost10
            (decode . encode) aclSeqPermitHost10 `shouldBe` Just aclSeqPermitHost10

        it "decode deny any ace-rule with sequence 20" $ do
            decode aclSeqJsonDenyAny20 `shouldBe` Just aclSeqDenyAny20
            (decode . encode) aclSeqDenyAny20 `shouldBe` Just aclSeqDenyAny20

    describe "ExtendedAclConf" $ do
        it "decode extended ACL configuration" $ do
            decode extendedAclConfJson `shouldBe` Just extendedAclConf
            (decode . encode) extendedAclConf `shouldBe` Just extendedAclConf

    describe "ExtendedAclConfBody" $ do
        it "decode extended ACL configuration body" $ do
            decode extendedAclConfBodyJson `shouldBe` Just extendedAclConfBody
            (decode . encode) extendedAclConfBody `shouldBe` Just extendedAclConfBody

    describe "AclIfConf" $ do
        it "decode ACL application to interface" $ do
            decode aclIfConfJson `shouldBe` Just aclIfConf
            (decode . encode) aclIfConf `shouldBe` Just aclIfConf

    describe "AclIfConfBody" $ do
        it "decode ACL application to interface" $ do
            decode aclIfConfBodyJson `shouldBe` Just aclIfConfBody
            (decode . encode) aclIfConfBody `shouldBe` Just aclIfConfBody

    describe "Flow" $ do
        it "decode flow" $ do
            eitherDecode flowJson1 `shouldBe` Right flow1
            (decode . encode) flow1 `shouldBe` Just flow1
            eitherDecode flowJson2 `shouldBe` Right flow2
            (decode . encode) flow2 `shouldBe` Just flow2
            eitherDecode flowJson3 `shouldBe` Right flow3
            (decode . encode) flow3 `shouldBe` Just flow3
            eitherDecode flowJson4 `shouldBe` Right flow4
            (decode . encode) flow4 `shouldBe` Just flow4

        it "decode flows" $ do
            eitherDecode flowsJson `shouldBe` Right flows
            (decode . encode) flows `shouldBe` Just flows

        it "decode cache" $ do
            eitherDecode cacheJson `shouldBe` Right cache
            (decode . encode) cache `shouldBe` Just cache

        it "decode monitor" $ do
            eitherDecode monitorJson `shouldBe` Right monitor
            (decode . encode) monitor `shouldBe` Just monitor
