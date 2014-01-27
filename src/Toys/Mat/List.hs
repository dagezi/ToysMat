module Toys.Mat.List
  where

-- |
-- Drop an element specified by index.
--
-- >>> dropByIndex 0 [1,2,3]
-- [2,3]
-- >>> dropByIndex 2 [1,2,3,4]
-- [1,2,4]
dropByIndex :: Int -> [a] -> [a]
dropByIndex _ [] = []
dropByIndex n (x:xs) 
     | n <= 0    = xs
     | otherwise = x : (dropByIndex (n - 1) xs)

-- |
-- Insert an element at position specified by index.
--
-- >>> insertAtIndex 0 0 [1,2,3]
-- [0,1,2,3]
-- >>> insertAtIndex 2 3 [1,2,4]
-- [1,2,3,4]
insertAtIndex :: Int -> a -> [a] -> [a]
insertAtIndex _ x []  = [x]
insertAtIndex n x xs | n <= 0  = x:xs
insertAtIndex n x (x':xs) = x' : (insertAtIndex (n - 1) x xs)
