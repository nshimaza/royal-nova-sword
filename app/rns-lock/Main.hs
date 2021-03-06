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
module Main where

import           Data.ByteString              (ByteString)
import           Network.HTTP.Simple          (Response)
import           Options.Applicative          (Parser, ParserInfo, argument,
                                               execParser, fullDesc, header,
                                               helper, info, metavar, progDesc,
                                               str, (<**>))
import           System.Exit                  (die)

import           Network.RoyalNovaSword
import           Network.RoyalNovaSword.Types

getHost :: Parser ByteString
getHost = argument str (metavar "HOST")

getUsername :: Parser ByteString
getUsername = argument str (metavar "USERNAME")

getPassword :: Parser ByteString
getPassword = argument str (metavar "PASSWORD")

getArgs :: Parser RestConfAccessProfile
getArgs = RestConfAccessProfile <$> getHost <*> getUsername <*> getPassword <*> pure False

argsInfo :: ParserInfo RestConfAccessProfile
argsInfo = info (getArgs <**> helper)
    (  fullDesc
    <> progDesc "Lock down switch based on collected flow cache."
    <> header   "rns-lock - Lock down switch based on collected flow cache."
    )

orDie :: Show a => String -> Either a b -> IO ()
orDie err (Left a) = die $ err <> show a
orDie _ _          = pure ()

interfaces :: [InterfaceId]
interfaces =
    [ InterfaceId GigabitEthernet "2/1"
    , InterfaceId GigabitEthernet "2/2"
    , InterfaceId GigabitEthernet "2/3"
    , InterfaceId GigabitEthernet "2/4"
    ]

main :: IO ()
main = do
    accessProfile <- execParser argsInfo
    res <- getFlowMonitorCache accessProfile
    orDie "getFlowMonitorCache" res
    setExtAcls accessProfile (toAclConf res) >>= orDie "setExtAcls"
    setAclInOnInterfaces accessProfile interfaces >>= orDie "setAclInOnInterfaces"
  where
    toAclConf :: Either (Response Monitor) Monitor -> [ExtendedAclConfBody]
    toAclConf (Right (Monitor (Cache _ (Flows flows)))) = makeExtAcls flows interfaces
