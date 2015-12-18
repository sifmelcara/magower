{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Communicate where

import MakeJSON
import ReadConfig

import Debug.Trace
import Data.Aeson
import Data.Maybe
import Control.Applicative
import Network.HTTP
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Network.HTTP.Conduit as NHC
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Data.CaseInsensitive
import Control.Exception

throwLink :: String -> 
             IO (Maybe TorID)  -- ^ return Nothing to try again in Main.
throwLink lk = do
    uri <- readURI
    sid <- genSesID
    res <- simpleHTTP . genPostReq uri sid $ mkAddContent lk
    getResponseCode res >>= \case
        (2, _, _) -> decode <$> getResponseBody res >>= \case
            Nothing -> error $ "Nothing in throwLink response!"
            Just Duplicate -> error $ "torrent duplicate!"
            Just x -> return . Just $ torID x
        (4, 0, 9) -> return Nothing -- confilict
        code    -> error $ "cannot recognize response code " ++ show code

getProgress :: TorID -> IO (Maybe Double)
getProgress tid = do
    uri <- readURI
    sid <- genSesID
    res <- simpleHTTP . genPostReq uri sid $ mkQurContent tid 
    getResponseCode res >>= \case
        (2, _, _) -> decode <$> getResponseBody res >>= \case
            Nothing -> error $ "NOthing in getProgress response!"
            Just x -> return . Just $ percentDone x
        (4, 0, 9) -> return Nothing
        code    -> error $ "cannot recognize response code" ++ show code

genSesID :: IO SesID
genSesID = do
    uri <- readURIString
    -- create a new manager everytime (bad code)
    manager <- NHC.newManager tlsManagerSettings
    -- the fromJust in the next line assume there is no failure when parseUrl
    rbs <- ((NHC.httpLbs (fromJust . NHC.parseUrl $ uri) manager)::IO (NHC.Response BL.ByteString)) `catch` xhandler
    traceShow rbs $ return ()
    --NHC.closeManager manager
    let sid = lookupSessionId $ NHC.responseHeaders rbs
    case sid of
        Nothing -> error "cannot generate session id"
        Just x -> return $ BC.unpack x
    where 
    lookupSessionId :: [Network.HTTP.Types.Header.Header] -> Maybe B.ByteString
    lookupSessionId [] = Nothing
    lookupSessionId ((name, ctnt):xs)
        | (name) == (mk ("X-Transmission-Session-Id")) = Just $ ctnt
        | otherwise = lookupSessionId xs
    xhandler (NHC.StatusCodeException status headers _) = do
        putStrLn "exception caught!"
        return $ return BL.empty


data Stat = Stat {dlProg :: String}

qurResToStat :: QurRes -> Stat
qurResToStat (QurRes pd) = Stat $ show pd


genPostReq :: URI -> SesID -> BL.ByteString -> Request BL.ByteString
genPostReq url sid cnt = (Request url POST hdrs cnt)
    where hdrs = [ mkHeader HdrContentType "Application/json"
                 , mkHeader HdrContentLength (show $ BL.length cnt)
                 , mkHeader sesHd sid
                 ]
          sesHd = HdrCustom "X-Transmission-Session-Id"



