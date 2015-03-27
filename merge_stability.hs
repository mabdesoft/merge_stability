{- Merge-sort algorithm from Rosetta Code
    but with a different approach to splitting
    that results in a stable sort -}

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

main = do
        let res :: [Record]
            res = [Record 5 "Apple"
                  ,Record 5 "Starfruit"
                  ,Record 5 "Melon"
                  ,Record 4 "Banana"
                  ,Record 3 "Pear"
                  ,Record 2 "Grape"
                  ,Record 1 "Pomegranate"
                  ]
        putStrLn "Unsorted:"
        putIntListLn res
        putStrLn "\nUnstable:"
        putIntListLn $ mergeSort usplit res
        putStrLn "\nStable:"
        putIntListLn $ mergeSort split res