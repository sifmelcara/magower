{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import GetLink
import Communicate
import ReadConfig
import ShowProgress

import Control.Monad
import Control.Applicative
import Data.Maybe
import System.Environment
import Control.Concurrent
import Control.Monad.Loops

main :: IO ()
main = do
    checkConfig
    link <- getLink
    tid <- untilJust $ throwLink link
    whileM_ (checkContinue tid) $ putPrg tid
    putStrLn "\nDownload finish!"
    return ()
    where putPrg t = do
            pgs <- untilJust $ getProgress t
            showProgress pgs
            threadDelay 500000
          checkContinue t = do
            pgs <- untilJust $ getProgress t
            return $ pgs < (1-1e-6)



