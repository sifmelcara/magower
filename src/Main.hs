{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Network.HTTP
import Network.URI

import Control.Monad
import Control.Applicative
import Data.Aeson
import Data.Maybe
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Concurrent

main :: IO ()
main = do
    link <- fmap head getArgs
    tid <- sentLnk link ""
    forever $ do
        sid <- getSesId
        st <- getStat tid sid
        putStrLn $ dlProg st
        threadDelay (1000000)
    return ()

sentLnk :: String -> SesID -> IO TorID
sentLnk lnk sid = do
    res <- simpleHTTP $ genPostReq remoteURI sid (encode . AddReq $ lnk)
    rbd <- getResponseBody res
    getResponseCode res >>= \case
        (2, _, _) -> decode <$> getResponseBody res >>= \case
            Nothing -> error $ ("Nothing in sentLnk resp: " ++ show rbd)
            Just Duplicate -> error "torrent duplicate!"
            Just x -> return $ torID x
        (4, 0, 9) -> sentLnk lnk =<< getSesId
        code      -> error $ "cannot recognize response code " ++ show code

getStat :: TorID -> SesID -> IO Stat
getStat tid sid = do 
    res <- simpleHTTP $ genPostReq remoteURI sid (encode $ QurReq [tid])
    rbd <- getResponseBody res
    getResponseCode res >>= \case
        (2, _, _) -> decode <$> getResponseBody res >>= \case
            Nothing -> do
                putStrLn $ "Nothing in getStat resp: " ++ show rbd
                putStrLn $ "tid = " ++ show tid
                return $ Stat ">/////<"
            Just x -> return $ qurResToStat x
        (4, 0, 9) -> getStat tid =<< getSesId
        code      -> error $ "cannot recognize response code " ++ show code

getSesId :: IO SesID
getSesId = do
    Right resp <- simpleHTTP $ getRequest remoteLink
    return $ fromJust . lookupHeader sesHd $ getHeaders resp 
    where sesHd = HdrCustom "X-Transmission-Session-Id"

type TorID = Integer
type SesID = String

data Stat = Stat {dlProg :: String}

qurResToStat :: QurRes -> Stat
qurResToStat (QurRes pd) = Stat $ show pd

data AddReq = AddReq { filename :: String
                     }
data AddRes = Duplicate | AddRes {torID :: TorID}
data QurReq = QurReq { ids :: [Integer]}
data QurRes = QurRes { percentDone :: Double}
                                
instance ToJSON AddReq where
    toJSON (AddReq name) = object ["method" .= ("torrent-add"::String), "arguments" .= object ["filename" .= name]] 
instance FromJSON AddRes where
    parseJSON (Object v) = do
        args <- v .: "arguments"
        args .:? "torrent-added" >>= \case
            Just x -> AddRes <$> (x .: "id")
            _   -> return Duplicate
    parseJSON v = error $ "FromJSON instance for AddRes get: " ++ show v
instance ToJSON QurReq where
    toJSON (QurReq tarID) = object ["method" .= ("torrent-get"::String), "arguments" .= object ["fields" .= ["percentDone"::String], "ids" .= tarID]]
instance FromJSON QurRes where
    parseJSON (Object v) = QurRes <$> ((\[x] -> x .: "percentDone") =<< ((.: "torrents") =<< (v .: "arguments")))
    parseJSON v = error $ "FromJSON instance for QurRes get: " ++ show v

remoteURI :: URI
remoteURI = fromJust . parseURI $ remoteLink
remoteLink :: String
remoteLink = "http://127.0.0.1:9091/transmission/rpc"

genPostReq :: URI -> SesID -> BL.ByteString -> Request BL.ByteString
genPostReq url sid cnt = (Request url POST hdrs cnt)
    where hdrs = [ mkHeader HdrContentType "Application/json"
                 , mkHeader HdrContentLength (show $ BL.length cnt)
                 , mkHeader sesHd sid
                 ]
          sesHd = HdrCustom "X-Transmission-Session-Id"


