main :: IO()
main = do

    print $ func3 [5, 7, 12]

func1 :: Int -> (Int -> Int)
func1 y = \x -> x * y

func2 :: (Double -> Double) -> (Double -> Double)
func2 f = \x -> f x

func3 :: [Int] -> [(Int, Int)]
func3 xs = zip xs [1..]