{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Network.RoyalNovaSwordSpec where

import           Test.Hspec

import           Network.RoyalNovaSword
import           Network.RoyalNovaSword.Types

spec :: Spec
spec = do
    let flow1 = Flow { flowSourceAddress      = "192.168.1.1"
                     , flowDestinationAddress = "10.1.1.1"
                     , flowInterfaceInput     = TwoLetterIfId $ InterfaceId GigabitEthernet "2/1"
                     }
        flowJson1 = "{\
                    \  \"source-address\": \"192.168.1.1\",\
                    \  \"destination-address\": \"10.1.1.1\",\
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
        aceRuleFlow1 = AceRule { aceRuleAction   = Permit
                               , aceRuleProtocol = Ip
                               , aceRuleHost     = Just "192.168.1.1"
                               , aceRuleDstHost  = Just "10.1.1.1"
                               , aceRuleAny      = Nothing
                               , aceRuleDstAny   = Nothing
                               }
        aclSeqFlow1 = AclSeqRule { aclSeqRuleSequence = "1"
                                 , aclSeqRuleAceRule = aceRuleFlow1
                                 }
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
        aceRuleFlow2 = AceRule { aceRuleAction   = Permit
                               , aceRuleProtocol = Ip
                               , aceRuleHost     = Just "192.168.1.1"
                               , aceRuleDstHost  = Just "192.168.1.2"
                               , aceRuleAny      = Nothing
                               , aceRuleDstAny   = Nothing
                               }
        aclSeqFlow2 = AclSeqRule { aclSeqRuleSequence = "2"
                                 , aclSeqRuleAceRule = aceRuleFlow2
                                 }
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
        aceRuleFlow3 = AceRule { aceRuleAction   = Permit
                               , aceRuleProtocol = Ip
                               , aceRuleHost     = Just "192.168.1.2"
                               , aceRuleDstHost  = Just "192.168.1.1"
                               , aceRuleAny      = Nothing
                               , aceRuleDstAny   = Nothing
                               }
        aclSeqFlow3 = AclSeqRule { aclSeqRuleSequence = "1"
                                 , aclSeqRuleAceRule = aceRuleFlow3
                                 }
        flow4 = Flow { flowSourceAddress      = "192.168.1.2"
                     , flowDestinationAddress = "10.1.1.1"
                     , flowInterfaceInput     = TwoLetterIfId $ InterfaceId GigabitEthernet "2/2"
                     }
        flowJson4 = "{\
                    \  \"source-address\": \"192.168.1.2\",\
                    \  \"destination-address\": \"10.1.1.1\",\
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
        aceRuleFlow4 = AceRule { aceRuleAction   = Permit
                               , aceRuleProtocol = Ip
                               , aceRuleHost     = Just "192.168.1.2"
                               , aceRuleDstHost  = Just "10.1.1.1"
                               , aceRuleAny      = Nothing
                               , aceRuleDstAny   = Nothing
                               }
        aclSeqFlow4 = AclSeqRule { aclSeqRuleSequence = "2"
                                 , aclSeqRuleAceRule = aceRuleFlow4
                                 }
        flows = [ flow1, flow2, flow3, flow4 ]
        aclSeqDenyAll = AclSeqRule "3" $ AceRule Deny Ip Nothing Nothing (Just [DummyNull]) (Just [DummyNull])
        extAclGi21 = ExtendedAclConfBody $ ExtendedAclConf "GigabitEthernet2/1" [ aclSeqFlow1, aclSeqFlow2, aclSeqDenyAll ]
        extAclGi22 = ExtendedAclConfBody $ ExtendedAclConf "GigabitEthernet2/2" [ aclSeqFlow3, aclSeqFlow4, aclSeqDenyAll ]


    describe "Flow to ACL converters" $ do
        it "flowToAceRule" $ do
            flowToAceRule flow1 `shouldBe` aceRuleFlow1
            flowToAceRule flow2 `shouldBe` aceRuleFlow2
            flowToAceRule flow3 `shouldBe` aceRuleFlow3
            flowToAceRule flow4 `shouldBe` aceRuleFlow4

        it "flowsForOneIfToAclSeqRules" $ do
            flowsForOneIfToAclSeqRules [ flow1, flow2 ] `shouldBe` [ aclSeqFlow1, aclSeqFlow2 ]
            flowsForOneIfToAclSeqRules [ flow3, flow4 ] `shouldBe` [ aclSeqFlow3, aclSeqFlow4 ]

        it "appendDenyAll" $ do
            (appendDenyAll . flowsForOneIfToAclSeqRules) [ flow1, flow2 ] `shouldBe` [ aclSeqFlow1, aclSeqFlow2, aclSeqDenyAll ]
            (appendDenyAll . flowsForOneIfToAclSeqRules) [ flow3, flow4 ] `shouldBe` [ aclSeqFlow3, aclSeqFlow4, aclSeqDenyAll ]

        it "makeExtAcl" $ do
            makeExtAcl flows (InterfaceId GigabitEthernet "2/1") `shouldBe` extAclGi21
            makeExtAcl flows (InterfaceId GigabitEthernet "2/2") `shouldBe` extAclGi22

        it "makeExtAcls" $ do
            makeExtAcls flows [ InterfaceId GigabitEthernet "2/1", InterfaceId GigabitEthernet "2/2" ]
                `shouldBe` [ extAclGi21, extAclGi22 ]
