{- Merge-sort algorithm from Rosetta Code
    but with a different approach to splitting
    that results in a stable sort.
    Since the original algorithm is covered by
    the GNU FDL v1.2, so is this. See fdl-1.2.txt.
 -}

import Control.Monad
import System.IO

data Record = Record { key :: Int
                     , value :: String
                     } deriving (Show)

merge []    ys                        = ys
merge xs    []                        = xs
merge xs@(x:xt) ys@(y:yt) | key x <= key y    = x : merge xt ys
                          | otherwise = y : merge xs yt

usplit (x:y:zs) = let (xs,ys) = usplit zs in (x:xs,y:ys)
usplit [x]      = ([x],[])
usplit []       = ([],[])

split s = let len = length s
              mid = quot len 2
              x = take mid s
              y = drop mid s in
          (x,y)

mergeSort splitFn [] = []
mergeSort splitFn [x] = [x]
mergeSort splitFn xs = let (as,bs) = splitFn xs
                       in merge (mergeSort splitFn as) (mergeSort splitFn bs)

putIntListLn = mapM_ (putStrLn . show)

askForPos :: IO Int
askForPos = do
    putStr "Position: "
    hFlush stdout
    pos <- getLine
    return $ read pos

askForName :: IO String
askForName = do
    putStr "Name: "
    hFlush stdout
    getLine

stopAdding :: IO Bool
stopAdding = do
    putStr "Add another? (y/n): "
    hFlush stdout
    resp <- getLine
    return $ resp == "n"

getRecords :: [Record] -> IO [Record]
getRecords recs = do
    pos <- askForPos
    name <- askForName
    let newRec = Record pos name
    halt <- stopAdding
    case halt of
        True -> return $ recs ++ [newRec]
        False -> getRecords (recs ++ [newRec])


main = do
    res <- getRecords []
    putStrLn "Unsorted:"
    putIntListLn res
    putStrLn "\nUnstable:"
    putIntListLn $ mergeSort usplit res
    putStrLn "\nStable:"
    putIntListLn $ mergeSort split res
