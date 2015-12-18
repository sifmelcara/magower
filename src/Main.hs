{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import GetLink
import Communicate
import ReadConfig
import ShowProgress

import Control.Concurrent
import Control.Monad.Loops

import qualified Network.HTTP.Conduit as NHC
import Network.HTTP.Client.TLS

main :: IO ()
main = do
    checkConfig
    link <- getLink
    manager <- NHC.newManager tlsManagerSettings
    let
        putPrg t = do
            pgs <- untilJust $ getProgress t manager 
            showProgress pgs
            threadDelay 500000
        checkContinue t = do
            pgs <- untilJust $ getProgress t manager 
            return $ pgs < (1-1e-6)
    tid <- untilJust $ throwLink link manager
    whileM_ (checkContinue tid) $ putPrg tid
    putStrLn "\nDownload finish!"
    return ()



