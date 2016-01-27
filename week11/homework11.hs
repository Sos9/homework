import Data.List

-- слепя цифрите a и b в числото ab
glue :: Int -> Int -> Int
glue a b = a * 10 + b

listToNumber :: [Int] -> Int
listToNumber l = foldl glue 0 l


-- връща броя на срещания на n в l
countN :: (Eq a) => a -> [a] -> Int
countN _ [] = 0
countN n l = length $ filter (\x -> x == n) l

-- преобразуваме първия списък като на мястото на всеки елемент
-- слагаме броя на срещанията му в l2
occurrences :: (Eq a) => [a] -> [a] -> [Int]
occurrences l1 l2 = map (\x -> countN x l2) l1


-- Броим от 0 
removeAt :: Int -> [a] -> [a]
removeAt 0 (_:xs) = xs
removeAt index l
  | (index >= length l) || index < 0  = error "Index out of bounds"
  | otherwise = concatT $ splitAt index l
  where
      concatT (firstPart, (_:xs)) = firstPart ++ xs
