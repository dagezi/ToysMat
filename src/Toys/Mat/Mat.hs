module Toys.Mat.Mat
  where

import Data.List
import Toys.Mat.List
import Toys.Mat.Field
import Toys.Mat.Vector

data Matrix f = Mat [Vector f]
  deriving (Eq, Read, Show)

instance (MatGroup f) => MatGroup (Matrix f) where
  dim (Mat vecs) = length vecs
  gzero n = Mat (take n $ repeat (gzero n))
  gadd (Mat vecs0) (Mat vecs1) = Mat (zipWith gadd vecs0 vecs1)
  gneg (Mat vecs) = Mat (map gneg vecs)

-- |
-- Matrix scalar multiplication
--
-- >>> msmul (Gr 2.0) (mone 2)
-- Mat [Vec [Gr 2.0,Gr 0.0],Vec [Gr 0.0,Gr 2.0]]
msmul :: (MatField f) => f -> Matrix f -> Matrix f
msmul a (Mat vecs) = Mat [vsmul a v | v <- vecs]

-- |
-- Matrix multiplication on Vector 
--
-- >>> mvmul (Mat [Vec [Gr 1, Gr 2], Vec [Gr 0, Gr 1]]) (Vec [Gr 1, Gr 2])
-- Vec [Gr 5.0,Gr 2.0]
mvmul :: (MatField f) => Matrix f -> Vector f -> Vector f
mvmul (Mat vecs) v = Vec (map (vdot v) vecs)

mvecs :: Matrix f -> [Vector f]
mvecs (Mat vecs) = vecs

melem :: Matrix f -> Int -> Int -> f
melem (Mat vecs) row col = velem (vecs !! row) col

mtrans :: Matrix f -> Matrix f
mtrans (Mat vecs) = Mat [Vec [velem v ix | v <- vecs] | ix <- [0 .. length vecs - 1]]

mmul :: (MatField f) => Matrix f -> Matrix f -> Matrix f
mmul m0 m1 = Mat [Vec [vdot v0 v1 | v1 <- mvecs (mtrans m1)] | v0 <- mvecs m0]

-- |
-- Identity matrix of size n
--
-- >>> (mone 2) :: (Matrix (Gr Double))
-- Mat [Vec [Gr 1.0,Gr 0.0],Vec [Gr 0.0,Gr 1.0]]
mone :: (MatField f) => Int -> Matrix f
mone n = Mat [Vec [if i == j then one else zero | i<-[0..n-1]] | j<-[0..n-1]]
  where
    zero = gzero 1
    one = fone 1

-- |
-- Determinant of maxrix
--
-- >>> mdet (Mat [Vec [Gr 2.0, Gr 1.0], Vec [Gr 2.0, Gr 3.0]])
-- Gr 4.0
-- >>> mdet (mone 3) :: (Gr Double)
-- Gr 1.0
mdet :: (MatField f) => Matrix f -> f
mdet (Mat []) = gzero 0
mdet (Mat [Vec [a]]) = a
mdet (Mat (v0 : vecs)) = 
  case findIndex ((gzero 1) /=) (velems v0) of
    Nothing -> gzero 1
    Just ix ->
      let 
        pivot = velem v0 ix
        pivotVec = vsmul (finv pivot) (Vec $ dropByIndex ix $ velems v0)
        sign = if ix == 0 then fone 1 else gneg (fone 1)
      in
        fmul (fmul pivot sign)
          (mdet (Mat [gsub (Vec (dropByIndex ix v)) (vsmul (v !! ix) pivotVec)
                       | Vec v <- vecs]))

-- |
-- Invert matrix.
--
-- >>> let m2One = mone 2 :: Matrix (Gr Double)
-- >>> minv (m2One) == m2One
-- True
-- >>> let a = Mat [Vec [Gr 1.0, Gr 2.0], Vec [Gr 1.0, Gr 3.0]]
-- >>> minv a
-- Mat [Vec [Gr 3.0,Gr (-2.0)],Vec [Gr (-1.0),Gr 1.0]]
minv :: (MatField f)=> Matrix f -> Matrix f
minv (Mat vecs) = 
  Mat (minvStep (length vecs - 1) vecs (mvecs (mone (length vecs))))

-- |
-- A step of invert matrix.
-- Find a row whose ix's element is not 0 and sweep whole matrix.
-- Apply this operation to 'ovs', too.
-- Repeat them until ix gets negative.
--
-- >>> let avecs = [Vec [Gr 1.0, Gr 2.0], Vec [Gr 1.0, Gr 3.0]]
-- >>> let evecs = mvecs (mone 2) :: [Vector (Gr Double)]
-- >>> minvStep (-1) avecs evecs == evecs
-- True
-- >>> minvStep (-1) evecs avecs == avecs
-- True
-- >>> minvStep 0 evecs evecs == evecs
-- True
minvStep :: (MatField f) => Int -> [Vector f] -> [Vector f] -> [Vector f]
minvStep ix ivs ovs 
  | ix < 0 = ovs
  | otherwise =
    case findIndex (\v -> velem v ix /= gzero 1) (take (ix + 1) ivs) of
      Nothing -> error "Invertible Matrix"
      Just pivotix ->
        let 
          coeffs = [velem v ix | v <- ivs]
        in
          minvStep
            (ix - 1)
            (sweepBy ivs ix pivotix coeffs)
            (sweepBy ovs ix pivotix coeffs)

-- |
-- sweep matrix and move pivot to specified index.
-- Coefficient at pivotIx is divider of pivot vector.
-- 
-- >>> let vecs = [Vec [Gr 2.0, Gr 1.0], Vec [Gr 2.0, Gr 4.0], Vec [Gr 3.0, Gr 4.0]]
-- >>> let ix = 2::Int
-- >>> let pivotIx = 1::Int
-- >>> let coeffs = [Gr 2.0, Gr 2.0, Gr 1.0]
-- >>> sweepBy vecs ix pivotIx coeffs
-- [Vec [Gr 0.0,Gr (-3.0)],Vec [Gr 2.0,Gr 2.0],Vec [Gr 1.0,Gr 2.0]]
sweepBy :: (MatField f) => [Vector f] -> Int -> Int -> [f] -> [Vector f]
sweepBy vecs ix pivotIx coeffs =
  insertAtIndex ix pivotVec
    [gsub vec (vsmul coeff pivotVec) | (vec, coeff)<-dropByIndex pivotIx $ zip vecs coeffs]
     where
       pivotVec = vsmul (finv (coeffs !! pivotIx)) (vecs !! pivotIx) 
