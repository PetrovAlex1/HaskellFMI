import Data.List

main :: IO()
main = do

    print (findJudge 2 [(1, 2)])
    print (findJudge 3 [(1, 3), (2, 3)] )
    print (findJudge 3 [(1, 3), (2, 3), (3, 1)] )
    print (findJudge 3 [(1, 2), (2, 3)])
    print (findJudge 4 [(1, 3), (1, 4), (2, 3), (2, 4), (4, 3)])


findJudge :: Int -> [(Int, Int)] -> Int
findJudge n xs = if null list then -1 else head list
    where
        list = [ p2 | (_, p2) <- xs, (trust p2 xs) == True]

trust :: Int -> [(Int, Int)] -> Bool
trust n xs = if length (helper n xs []) == length (nub $ map (fst) xs) then True else False 
    where
        helper _     []        result        = result
        helper n ((p1, p2):xs) result
            | p2 == n && notElem (p1) result = helper n xs (p1:result)
            | otherwise                      = helper n xs result 