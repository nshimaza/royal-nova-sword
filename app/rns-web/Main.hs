{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import qualified Data.ByteString              as S (ByteString)
import qualified Data.ByteString.Lazy         as L (ByteString)
import qualified Data.ByteString.Lazy.Char8   as L8 (pack)
import           Network.HTTP.Simple          (Response)
import           Network.Wai.Handler.Warp     (run)
import           Options.Applicative          (Parser, ParserInfo, argument,
                                               execParser, fullDesc, header,
                                               helper, info, metavar, progDesc,
                                               str, (<**>))
import           Servant

import           Network.RoyalNovaSword
import           Network.RoyalNovaSword.Types

getHost :: Parser S.ByteString
getHost = argument str (metavar "HOST")

getUsername :: Parser S.ByteString
getUsername = argument str (metavar "USERNAME")

getPassword :: Parser S.ByteString
getPassword = argument str (metavar "PASSWORD")

getArgs :: Parser RestConfAccessProfile
getArgs = RestConfAccessProfile <$> getHost <*> getUsername <*> getPassword <*> pure False

argsInfo :: ParserInfo RestConfAccessProfile
argsInfo = info (getArgs <**> helper)
    (  fullDesc
    <> progDesc "Unlock switch and start learning flow."
    <> header   "rns-learn - Unlock switch and start learning flow."
    )

orDie :: Show a => L.ByteString -> Either a b -> Handler ()
orDie err (Left a) = throwError $ err500 { errBody = err <> ": " <> (L8.pack $ show a) }
orDie _ _          = pure ()

interfaces :: [InterfaceId]
interfaces =
    [ InterfaceId GigabitEthernet "2/1"
    , InterfaceId GigabitEthernet "2/2"
    , InterfaceId GigabitEthernet "2/3"
    , InterfaceId GigabitEthernet "2/4"
    ]

type UserAPI1 = "api" :>
    (
         PostNoContent '[PlainText] NoContent
    :<|> PutNoContent '[PlainText] NoContent
    :<|> DeleteNoContent '[PlainText] NoContent
    )

learn :: RestConfAccessProfile -> [InterfaceId] -> Handler NoContent
learn prof ifs = do
    unlock prof ifs
    setNetFlowConf prof >>= orDie "setNetFlowConf"
    setNetFlowMonOnInterfaces prof ifs >>= orDie "setNetFlowMonOnInterfaces"
    pure NoContent

lock :: RestConfAccessProfile -> [InterfaceId] -> Handler NoContent
lock prof ifs = do
    res <- getFlowMonitorCache prof
    orDie "getFlowMonitorCache" res
    setExtAcls prof (toAclConf res) >>= orDie "setExtAcls"
    setAclInOnInterfaces prof ifs >>= orDie "setAclInOnInterfaces"
    pure NoContent
  where
    toAclConf :: Either (Response Monitor) Monitor -> [ExtendedAclConfBody]
    toAclConf (Right (Monitor (Cache _ (Flows flows)))) = makeExtAcls flows interfaces

unlock :: RestConfAccessProfile -> [InterfaceId] -> Handler NoContent
unlock prof ifs = do
    deleteAclInOnInterfaces prof ifs >>= orDie "deleteAclInOnInterfaces"
    deleteAllAcls prof >>= orDie "deleteAllAcls"
    deleteNetFlowMonOnInterfaces prof ifs >>= orDie "deleteNetFlowMonOnInterfaces"
    deleteNetFlowConf prof >>= orDie "deleteNetFlowConf"
    pure NoContent

server1 :: RestConfAccessProfile -> [InterfaceId] -> Server UserAPI1
server1 prof ifs = learn prof ifs :<|> lock prof ifs :<|> unlock prof ifs

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: RestConfAccessProfile -> [InterfaceId] -> Application
app1 prof ifs = serve userAPI $ server1 prof ifs

main :: IO ()
main = do
    accessProfile <- execParser argsInfo
    run 8081 $ app1 accessProfile interfaces
