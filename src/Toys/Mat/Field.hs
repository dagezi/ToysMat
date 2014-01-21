module Toys.Mat.Field
  where

class Eq g => MatGroup g where
  dim :: g -> Int
  gzero :: Int -> g
  gadd, gsub :: g -> g -> g
  gneg :: g -> g
  gsub a b  = gadd a (gneg b)

class MatGroup f => MatField f where
  fone :: Int -> f
  fmul, fdiv :: f -> f -> f
  finv :: f -> f
  fdiv a b  = fmul a (finv b)

data Gr a = Gr a
  deriving (Eq, Ord, Read, Show)

instance (Num a, Eq a) => MatGroup (Gr a) where
  dim g = 0
  gzero n = Gr 0
  gadd (Gr a) (Gr b) = Gr (a + b)
  gneg (Gr a) = Gr (-a)

instance (Fractional a, Eq a) => MatField (Gr a) where
  fone n = Gr 1
  fmul (Gr a) (Gr b) = Gr (a * b)
  finv (Gr a) = Gr (1 / a)

