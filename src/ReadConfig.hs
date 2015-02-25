{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase#-}

module ReadConfig where

import Network.URI
import Data.Maybe
import System.Directory
import System.FilePath
import qualified Data.Configurator as C

readURI :: IO URI
readURI = do
    config <- C.load [C.Required "$(HOME)/.magower"]
    C.lookup config "servpath" >>= \case
        Just servpath -> return . fromJust $ parseURI servpath
        Nothing -> error "please edit config file(.magower)."

checkConfig :: IO ()
checkConfig = do
    homedir <- getHomeDirectory 
    let configFile = homedir </> ".magower"
    doesFileExist configFile >>= \case
        True -> return ()
        False -> writeFile configFile defaultConfig


defaultConfig :: String
defaultConfig = unlines [ ""
                        , "# This is a config file for program magower"
                        , ""
                        , "servpath = \"http://127.0.0.1:9091/transmission/rpc\""
                        , ""
                        ]

