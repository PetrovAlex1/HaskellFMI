main :: IO()
main = do

    print (persistence 39)
    print (persistence 999)
    print (persistence 126)
    print (persistence 4)


persistence :: Int -> (Int, [Int])
persistence num = helper num 0 []
        where
            helper num i xs
                | i > 0 && head xs > 0 && head xs < 10 = (length xs , (reverse xs)) 
                | i == 0                      = helper num (i + 1) ((multiplyElements (splitNumber num)) : xs)
                | otherwise                   = helper num (i + 1) ((multiplyElements (splitNumber (head xs))) : xs) 

splitNumber :: Int -> [Int]
splitNumber num = helper num []
        where
            helper num xs
                | num == 0  = xs
                | otherwise = helper (num `div` 10) ((num `mod` 10) : xs)

multiplyElements :: [Int] -> Int
multiplyElements xs = if null xs then 1 else head xs * (multiplyElements (tail xs))  