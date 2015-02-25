{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


import Control.Monad
import Control.Applicative
import Data.Maybe
import System.Environment
import Control.Concurrent

main :: IO ()
main = do
    checkConfig
    link <- GetLink
