main :: IO()
main = do

    print ("1.Task")
    print (countRats  ")1)1)1)1 P")
    print (countRats  "P 1( 1( )1 1(")
    print (countRats  " P 1( 1( )1 1(")
    print (countRats  ")1)1)1)1P)1)11(")
    print (countRats  "1()1)1)11(1()1)1P)11()1)1)11(1(1(1(")
    print (countRats "")
    print ("2.Task")
    print ((josephus [1,2,3,4,5,6,7]) 3)
    print ((josephus [1,2,3,4,5,6,7]) (-1))
    print ((josephus [1,2,3,4,5,6,7,8,9,10]) 1)
    print ((josephus [1,2,3,4,5,6,7,8,9,10]) 2)
    print ((josephus "fpFMIsu") 4)

--1.TASK
countRats :: [Char] -> Int
countRats [] = 0
countRats xs = helper xs [] 0 
    where
        helper xs cs counter
                | head xs /= 'P'                   = helper (tail xs) ((head xs) : cs) counter 
                | head xs == 'P'                   = (countScope cs '(') + (countScope xs ')')
                | otherwise                        = helper (tail xs) cs counter

countScope :: [Char] -> Char -> Int
countScope xs scope = helper xs scope 0
    where
        helper xs scope counter
                | null xs        = counter
                | head xs == scope = helper (tail xs) scope (counter + 1)
                | otherwise      = helper  (tail xs) scope counter

--2.TASK

josephus :: [a] -> (Int -> [a])
josephus xs = \ k -> createOrder xs k

createOrder :: [a] -> Int -> [a] -- creates the oreder of the children
createOrder xs k | k < 0 = error "k was not natural"
createOrder xs k = helper k xs []
    where
        helper k xs rs = if null xs then rs else helper k (tail (reorder (k - 1) xs)) (rs ++ [head (reorder (k - 1) xs)])

reorder :: Int -> [a] -> [a] -- this function reorder the list so that the element, whose turn is to be out of the circle, to be in front of the list
reorder n xs = helper n xs 0
    where
        helper n xs i
              | i < n     = helper n (((tail xs) ++ [(head xs)])) (i + 1) 
              | otherwise = xs