isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n
                
                
truncatablePrime :: Int -> Bool
truncatablePrime x 
     | x < 10 = isPrime x
     | otherwise = (isPrime x) && (truncatablePrime (div x 10))

     checkNumberIn :: Int -> Int -> Bool
checkNumberIn n x
     | n < 10    =   n == x
     | otherwise = ((mod n 10) == x) || (checkNumberIn (div n 10) x)



containsDigits :: Int -> Int -> Bool
containsDigits n m 
     | m < 10  =  (checkNumberIn n m)
     | otherwise = (checkNumberIn n (mod m 10)) && (containsDigits n (div m 10))

     productOfDigits :: Int -> Int
productOfDigits n 
     | n < 10 =  n
     | otherwise = (mod n 10) * (productOfDigits (div n 10))




     divisors :: Int -> [Int]
divisors n = 1 : filter ((==0) . rem n) [2 .. n `div` 2]

sumDivisors :: (Num a) => [a] -> a  
sumDivisors xs = foldl (\acc x -> acc + x) 0 xs  

dFn :: Int -> Int
dFn n = (sumDivisors (divisors n))


interestingNumber :: Int -> Bool
interestingNumber n =   n == (dFn (dFn n))


quadrant :: Double -> Double -> Int
quadrant x y 
     | x == 0 && y == 0  =  0
     | x > 0  && y > 0   =  1
     | x < 0  && y > 0   =  2
     | x < 0  && y < 0   =  3
     | x > 0  && y < 0   =  4
