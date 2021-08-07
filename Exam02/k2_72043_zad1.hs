main :: IO()
main = do

    print ((getIndices [2, 7, 11, 15]) 9)
    print ((getIndices [3, 2, 4]) 6)
    print ((getIndices [3, 3]) 6)

getIndices :: [Int] -> (Int -> (Int, Int))
getIndices xs = \x -> findIndexes xs x

findIndexes :: [Int] -> Int -> (Int, Int)
findIndexes xs n = helper xs n 0
    where
        helper (x:xs) n i
            | x + (head xs) == n = (i, (i + 1))
            | otherwise          = helper xs n (i + 1)            