
module Communicate where

import Network.HTTP
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as BL

throwLink :: String -> IO ()
throwLink lk = do
    uri <- readURI
    sid <- genSesID
    res <- simpleHTTP . genPostReq uri sid $ 

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

genSesID :: IO SesID
genSesID = do
    Right resp <- simpleHTTP $ getRequest remoteLink
    return $ fromJust . lookupHeader sesHd $ getHeaders resp 
    where sesHd = HdrCustom "X-Transmission-Session-Id"


data Stat = Stat {dlProg :: String}

qurResToStat :: QurRes -> Stat
qurResToStat (QurRes pd) = Stat $ show pd


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



