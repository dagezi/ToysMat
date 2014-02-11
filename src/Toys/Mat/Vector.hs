module Toys.Mat.Vector
  where

import Toys.Mat.Field

-- $setup
-- >>> import Data.Ratio

data Vector f = Vec [f]
  deriving (Eq, Read, Show)

instance (MatGroup f) => MatGroup (Vector f) where
  dim (Vec a) = length a
  gzero n = Vec (take n $ repeat (gzero 1))
  gadd (Vec a) (Vec b) = Vec (zipWith gadd a b)
  gneg (Vec a) = Vec (map gneg a)

-- |
-- Vector's scalar multiplication
--
-- >>> vsmul (-1.0) (Vec [1.0, 2.0])
-- Vec [-1.0,-2.0]
vsmul :: (MatField f) => f -> Vector f -> Vector f
vsmul a (Vec v)  = Vec (map (fmul a) v)

-- |
-- Vector's dot product
--
-- >>> vdot (Vec [1.0,2.0]) (Vec [-2.0,1.0])
-- 0.0
-- >>> vdot (Vec [1%2,3%2]) (Vec [2,4])
-- 7 % 1
vdot :: (MatField f) => Vector f -> Vector f -> f
vdot (Vec v0) (Vec v1)  = foldl gadd (gzero 1) (zipWith fmul v0 v1)

velem :: Vector f -> Int -> f
velem (Vec v) n = v !! n

velems :: Vector f -> [f]
velems (Vec v) = v

