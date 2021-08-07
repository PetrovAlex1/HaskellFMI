main :: IO()
main = do

    print (sumOfEvenly 1 10)
    print (sumOfEvenly 5 20)

sumOfEvenly :: Int -> Int -> Int
sumOfEvenly a b = sum [x | x <- [a .. b], checkEvenly x == True]

checkEvenly :: Int -> Bool
checkEvenly num = helper num 2 1
    where
        helper num i counter
                    | i > num      = if counter `mod` 2 == 0 then True else False
                    | num `mod` i == 0 = helper num (i + 1) (counter + 1)
                    | otherwise        = helper num (i + 1) counter 