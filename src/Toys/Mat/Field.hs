module Toys.Mat.Field
  where

import Data.Ratio

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

instance MatGroup Int where
  dim _ = 1
  gzero _ = 0
  gadd = (+)
  gsub = (-)
  gneg = negate

instance MatGroup Integer where
  dim _ = 1
  gzero _ = 0
  gadd = (+)
  gsub = (-)
  gneg = negate

instance MatGroup Float where
  dim _ = 1
  gzero _ = 0
  gadd = (+)
  gsub = (-)
  gneg = negate

instance MatField Float where
  fone _ = 1
  fmul = (*)
  fdiv = (/)
  finv = recip

instance MatGroup Double where
  dim _ = 1
  gzero _ = 0
  gadd = (+)
  gsub = (-)
  gneg = negate

instance MatField Double where
  fone _ = 1
  fmul = (*)
  fdiv = (/)
  finv = recip

instance (Eq a, Integral a) => MatGroup (Ratio a) where
  dim _ = 1
  gzero _ = 0 % 1
  gadd = (+)
  gsub = (-)
  gneg = negate

instance (Eq a, Integral a) => MatField (Ratio a) where
  fone _ = 1 % 1
  fmul = (*)
  fdiv = (/)
  finv = recip
