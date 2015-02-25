{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import GetLink
import Communicate
import ReadConfig

import Control.Monad
import Control.Applicative
import Data.Maybe
import System.Environment
import Control.Concurrent

main :: IO ()
main = do
    checkConfig
    link <- getLink
    tid <- untilJust $ throwLink link
    forever $ putStrLn . show =<< (untilJust $ getProgress tid)
    return ()
    

untilJust :: Monad m => m (Maybe a) -> m a
untilJust m = m >>= \case
    Nothing -> untilJust m
    Just x -> return x
        
