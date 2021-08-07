import Data.List

main :: IO()
main = do

    print ((kthMaxMin [-1]) 1)
    print ((kthMaxMin [-1,-5,-6,-6,-6,-6]) 2)
    print ((kthMaxMin [1,2,3,4,-5,6,7,-2,-1,0]) 2)
    print ((kthMaxMin [-1,0,-1,0,-2,3,1,-1]) 3)

kthMaxMin :: [Int] -> (Int -> Int)
kthMaxMin xs = \x -> printNumber (sort $ nub ([abs x | x <- xs, x < 0])) x

printNumber :: [Int] -> Int -> Int
printNumber [] k = error "No such number"
printNumber xs k = if k <= length xs then -(xs !! (k - 1)) else error "No such number"