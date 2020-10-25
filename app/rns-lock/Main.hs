{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

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
