{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Communicate where

import MakeJSON
import ReadConfig

import Debug.Trace
import Data.Aeson
import Control.Applicative
import Network.HTTP
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Network.HTTP.Conduit as NHC
import Network.HTTP.Types.Header
import Data.CaseInsensitive
import Network.HTTP.Types.Status

throwLink :: String -> 
             NHC.Manager -> 
             IO (Maybe TorID)  -- ^ return Nothing to try again in Main.
throwLink lk manager = do
    uri <- readURIString
    sid <- genSesID manager
    iniReq <- NHC.parseUrl uri
    let req = iniReq
              { NHC.method = "POST"
              , NHC.requestBody = NHC.RequestBodyLBS $ mkAddContent lk
              , NHC.requestHeaders = genReqHeaders sid
              }
    let uriT = drop 2 . dropWhile (/= '/') $ uri
    let areq = case parseURIAuthority uriT of
                Nothing -> req
                Just auth -> case user auth of
                    Nothing -> req
                    Just usern -> case password auth of
                        Nothing -> error "only username present in config file, but not password"
                        Just pass -> NHC.applyBasicAuth (BC.pack usern) (BC.pack pass) req
    traceShow areq $ return ()
    rbs <- NHC.httpLbs areq manager :: IO (NHC.Response BL.ByteString) 
    traceShow "server response:" $ return ()
    traceShow rbs $ return ()
    case stcp . statusCode $ NHC.responseStatus rbs of
        (2, _, _) -> case decode (NHC.responseBody rbs) of
            Nothing -> error "Nothing in throwLink response!"
            Just Duplicate -> error "torrent duplicate!"
            Just x -> return . Just $ torID x
        (4, 0, 9) -> return Nothing -- confilict
        code    -> error $ "throwLink: cannot recognize response code " ++ show code
    where
    stcp :: Int -> (Int, Int, Int)
    stcp i = let s::String = show i in (read $ [s !! 0], read $ [s !! 1], read $ [s !! 2])

getProgress :: TorID -> NHC.Manager -> IO (Maybe Double)
getProgress tid manager = do
    uri <- readURIString
    sid <- genSesID manager
    iniReq <- NHC.parseUrl uri
    let req = iniReq
              { NHC.method = "POST"
              , NHC.requestBody = NHC.RequestBodyLBS $ mkQurContent tid
              , NHC.requestHeaders = genReqHeaders sid
              }
    let uriT = drop 2 . dropWhile (/= '/') $ uri
    let areq = case parseURIAuthority uriT of
                Nothing -> req
                Just auth -> case user auth of
                    Nothing -> req
                    Just usern -> case password auth of
                        Nothing -> error "only username present in config file, but not password"
                        Just pass -> NHC.applyBasicAuth (BC.pack usern) (BC.pack pass) req
    rbs <- NHC.httpLbs areq manager :: IO (NHC.Response BL.ByteString)
    case stcp . statusCode $ NHC.responseStatus rbs of
        (2, _, _) -> case decode $ NHC.responseBody rbs of
            Nothing -> error $ "NOthing in getProgress response!"
            Just x -> return . Just $ percentDone x
        (4, 0, 9) -> return Nothing
        code    -> error $ "getPregress: cannot recognize response code " ++ show code
    where
    stcp :: Int -> (Int, Int, Int)
    stcp i = let s::String = show i in (read [s !! 0], read [s !! 1], read [s !! 2])

genSesID :: NHC.Manager -> IO SesID
genSesID manager = do
    uri <- readURIString
    iniReq <- NHC.parseUrl uri
    let req = iniReq
    rbs <- NHC.httpLbs req manager :: IO (NHC.Response BL.ByteString)
    let sid = lookupSessionId $ NHC.responseHeaders rbs
    case sid of
        Nothing -> error "cannot generate session id"
        Just x -> return $ BC.unpack x
    where 
    lookupSessionId :: [Network.HTTP.Types.Header.Header] -> Maybe B.ByteString
    lookupSessionId [] = Nothing
    lookupSessionId ((name, ctnt):xs)
        | name == mk "X-Transmission-Session-Id" = Just ctnt
        | otherwise = lookupSessionId xs


data Stat = Stat {dlProg :: String}

qurResToStat :: QurRes -> Stat
qurResToStat (QurRes pd) = Stat $ show pd


genReqHeaders :: SesID -> [Network.HTTP.Types.Header.Header] 
genReqHeaders sid = [ (mk "X-Transmission-Session-Id", BC.pack sid)
                    , (mk "Content-Type", "Application/json")
                    ]



