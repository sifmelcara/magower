{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase#-}

module ReadConfig where

import Network.URI
import Data.Maybe
import qualified Data.Configurator as C

readURI :: IO URI
readURI = do
    config <- C.load [C.Required "$(HOME)/.magower"]
    C.lookup config "servpath" >>= \case
        Just servpath -> return . fromJust $ parseURI servpath
        Nothing -> error "please edit config file(.magower)."

