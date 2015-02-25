{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


import Control.Monad
import Control.Applicative
import Data.Maybe
import System.Environment
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

