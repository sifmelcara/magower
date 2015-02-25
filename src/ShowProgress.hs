
module ShowProgress where

import System.IO
import Text.Printf

showProgress :: Double -> IO ()
showProgress d = putProgress $ genBar d ++ " " ++ (printf "%3d%%" $ ((truncate (d*100))::Int))

putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r\ESC[K" ++ s

genBar :: Double -> String
genBar d = "[" ++ brs ++ ">" ++ ws ++ "]"
    where brs = replicate eqnum '='
          eqnum = round $ d*50
          ws = replicate (50-eqnum) ' '
