{-# LANGUAGE OverloadedStrings #-}
{-# LAGUAGE LambdaCase #-}

module MakeJSON where

import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BL

type TorID = Integer
type SesID = String

mkAddContent :: String -> BL.ByteString
mkAddContent lnk = encode $ AddReq lnk

mkQurContent :: TorID -> BL.ByteString
mkQurContent tid = encode $ QurReq [tid]

decode

data AddReq = AddReq { filename :: String }
data AddRes = Duplicate | AddRes { torID :: TorID }
data QurReq = QurReq { ids :: [Integer] }
data QurRes = QurRes { percentDone :: Double }
                                
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

