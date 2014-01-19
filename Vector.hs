module Vector
  where

import Data.List
import Data.Ratio
import Field

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
-- >>> vsmul (Gr (-1.0)) (Vec [Gr 1.0, Gr 2.0])
-- Vec [Gr (-1.0),Gr (-2.0)]
vsmul :: (MatField f) => f -> Vector f -> Vector f
vsmul a (Vec v)  = Vec (map (fmul a) v)

-- |
-- Vector's dot product
--
-- >>> vdot (Vec [Gr 1.0, Gr 2.0]) (Vec [Gr (-2.0), Gr 1.0])
-- Gr 0.0
-- >>> vdot (Vec [Gr (1%2), Gr (3%2)]) (Vec [Gr 2, Gr 4])
-- Gr (7 % 1)
vdot :: (MatField f) => Vector f -> Vector f -> f
vdot (Vec v0) (Vec v1)  = foldl gadd (gzero 1) (zipWith fmul v0 v1)

velem :: Vector f -> Int -> f
velem (Vec v) n = v !! n

velems :: Vector f -> [f]
velems (Vec v) = v

