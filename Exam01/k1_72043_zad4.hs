import Text.Read

main :: IO()
main = do

    print (scoreHand ["A"])
    print (scoreHand ["A", "J"])
    print (scoreHand ["5","3","7"])
    print (scoreHand ["5","4","3","2","A","K"])
    print (scoreHand ["2","3"])
    print (scoreHand ["4","5","6"])
    print (scoreHand ["7","7","8"])
    print (scoreHand ["K","J","Q"])
    print (scoreHand ["A","3"])
    print (scoreHand ["A","A"])
    print (scoreHand ["A", "10", "A"])
    print (scoreHand ["A","2","A","9","9"])

chars = ["J", "Q", "K"]

scoreHand :: [String] -> Int
scoreHand xs = helper xs 0 0
    where
        helper xs score countFindA
            | null xs                         = validateScore score countFindA 
            | head xs == "A"                  = helper (tail xs) (11 + score) (countFindA + 1)
            | notElem (head xs) chars == True = helper (tail xs) ((read (head xs) :: Int) + score) countFindA
            | otherwise                       = helper (tail xs) (10 + score) countFindA

validateScore :: Int -> Int -> Int
validateScore score countFindA = helper score countFindA         
            where
                helper score countFindA
                        | countFindA > 0 = if score <= 21 then score else helper (score - 10) (countFindA - 1)
                        | otherwise     = score  