{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Main where

import           Data.ByteString              (ByteString)
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
    <> progDesc "Reset all NetFlow monitor and ACL."
    <> header   "rns-reset - Reset all NetFlow monitor and ACL."
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
    deleteAclInOnInterfaces accessProfile interfaces >>= orDie "deleteAclInOnInterfaces"
    deleteAllAcls accessProfile >>= orDie "deleteAllAcls"
    deleteNetFlowMonOnInterfaces accessProfile interfaces >>= orDie "deleteNetFlowMonOnInterfaces"
    deleteNetFlowConf accessProfile >>= orDie "deleteNetFlowConf"
