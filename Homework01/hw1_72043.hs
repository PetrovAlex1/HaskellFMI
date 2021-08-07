main :: IO()
main = do
    
    print ("1 Task")
    print ("a)")
    print (safePrimeCount 20 100)
    print (safePrimeCount 1 983)
    print (safePrimeCount 167 1892)
    print (safePrimeCount 1678 20097)
    print ("----")
    print ("b)")
    print (specialSum 3 20)
    print (specialSum 5 31)
    print (specialSum 8 10)
    print (specialSum 10 128)
    print ("2 Task")
    print (validate 1714)
    print (validate 12345)
    print (validate 891)
    print (validate 123)
    print (validate 2121)
    print (validate 4736778291034)
    print (validate 4485756008412)
    print (validate 4485756008422)
    print (validate 4214154976719)
    
safePrimeCount :: Integer -> Integer -> Integer
safePrimeCount a b = helper a b 0
    where
        helper a b counter
                        | a == 1    = helper (a + 1) b counter --check if a is 1 and continue
                        | a > b    = counter
                        | isPrime ((a - 1) `div` 2) && isPrime a = helper (a + 1) b (counter + 1) --check if the current number (a) is Safe prime
                        | otherwise = helper (a + 1) b counter

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n  =  helper n 2
    where
        helper n i
                | (mod n i) == 0   = False
                | i >= (div n 2)   = True
                | otherwise        = helper n (i + 1)   

specialSum :: Integer -> Integer -> Integer
specialSum k m = specHelper k m 2 0
        where
            specHelper k m degree sum
                | isPrime degree && (2 ^ degree - 1) > m    = specHelper (k - 1) m (degree + 1) (sum + (2 ^ degree - 1))
                | k == 0                                    = sum
                | otherwise                                 = specHelper k m (degree + 1) sum


--2 TASK    
validate :: Integer -> Bool
validate num = processNumber num 1 1 0
    where
        processNumber num i d result --'i' will be our iterator for the digits of the number; 'result' will be the 'num', but with double digits;
                                | i == 1            = processNumber (num `div` 10) (i + 1) (d * 10) (num `mod` 10)
                                | i `mod` 2 == 0    = processNumber (num `div` 10) (i + 1) (d * 10) (result + (doubleDigit (num `mod` 10)) * d)--check for every even digit from right to left
                                | num == 0          = checkSumOfDigits result
                                | num `mod` 10 == 0 = processNumber (num `div` 10) (i + 1) (d * 10) result--check if current digit is 0
                                | otherwise         = processNumber (num `div` 10) (i + 1) (d * 10) (result + (num `mod` 10) * d )

doubleDigit :: Integer -> Integer 
doubleDigit digit = if (digit * 2) > 9 then ((digit * 2) `mod` 10) + ((digit * 2)) `div` 10 else digit * 2  

checkSumOfDigits :: Integer -> Bool
checkSumOfDigits result = checkHelper result 0
    where
        checkHelper result sum 
                            | (result == 0) && (sum `mod` 10 == 0) = True
                            | (result == 0) && (sum `mod` 10 /= 0) = False
                            | otherwise = checkHelper (result `div` 10) (sum + (result `mod` 10))