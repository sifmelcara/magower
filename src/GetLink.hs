
module GetLink where

getLink :: IO String
getLink = do
    putStrLn "Please feed me a magnet link:"
    getLine

