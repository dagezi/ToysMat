module Mat
  where

import Data.List
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

data Matrix f = Mat [(Vector f)]
  deriving (Eq, Read, Show)

instance (MatGroup f) => MatGroup (Matrix f) where
  dim (Mat m) = length m
  gzero n = Mat (take n $ repeat (gzero n))
  gadd (Mat m0) (Mat m1) = Mat (zipWith gadd m0 m1)
  gneg (Mat m) = Mat (map gneg m)

-- |
-- Matrix multiplication on Vector 
--
-- >>> mvmul (Mat [Vec [Gr 1, Gr 2], Vec [Gr 0, Gr 1]]) (Vec [Gr 1, Gr 2])
-- Vec [Gr 5.0,Gr 2.0]
mvmul :: (MatField f) => Matrix f -> Vector f -> Vector f
mvmul (Mat m) v = Vec (map (vdot v) m)

mvecs :: Matrix f -> [Vector f]
mvecs (Mat m) = m

melem :: Matrix f -> Int -> Int -> f
melem (Mat m) row col = velem (m !! row) col

mtrans :: Matrix f -> Matrix f
mtrans (Mat m) = Mat [Vec [velem v ix | v <- m] | ix <- [0 .. length m - 1]]

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
mdet (Mat [Vec [a]]) = a
mdet (Mat (v0 : vs)) = 
  case findIndex ((gzero 1) /=) (velems v0) of
    Nothing -> gzero 1
    Just ix ->
      let 
        pivot = velem v0 ix
        pivotv = vsmul (finv pivot) (Vec $ removeAt ix $ velems v0) 
        sign = if ix == 0 then fone 1 else gneg (fone 1)
      in
        fmul (fmul pivot sign)
          (mdet (Mat [gsub (Vec (removeAt ix v)) (vsmul (v !! ix) pivotv)  
                       | Vec v <- vs]))
  where 
    removeAt _ [] = []
    removeAt n (v:vs) | n == 0  = vs
    removeAt n (v:vs) = v : removeAt (n - 1) vs

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
minv (Mat vs) = 
  Mat (minvStep (length vs - 1) vs (mvecs (mone (length vs))))

-- |
-- A step of invert matrix.
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
          pivot = velem (ivs !! pivotix) ix
          ipivotv = vsmul (finv pivot) (ivs !! pivotix) 
          opivotv = vsmul (finv pivot) (ovs !! pivotix)
          ivrest = dropByIndex pivotix ivs
          ovrest = dropByIndex pivotix ovs
          vals = [velem v ix | v <- ivrest]
        in
          minvStep
            (ix - 1)
            (insertAtIndex ix ipivotv 
               (sweepBy ivrest ipivotv vals))
            (insertAtIndex ix opivotv 
               (sweepBy ovrest opivotv vals))

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
insertAtIndex n x (x':xs) = x': (insertAtIndex (n - 1) x xs)

-- |
-- sweep matrix
-- 
-- >>> let m = [Vec [Gr 2.0, Gr 1.0], Vec [Gr 3.0, Gr 4.0]]
-- >>> let pivotv = Vec [Gr 1.0, Gr 2.0]
-- >>> let vals = [Gr 2.0, Gr 1.0]
-- >>> sweepBy m pivotv vals
-- [Vec [Gr 0.0,Gr (-3.0)],Vec [Gr 2.0,Gr 2.0]]
sweepBy :: (MatField f) => [Vector f] -> Vector f -> [f] -> [Vector f]
sweepBy vecs pivotv vals =
  zipWith (\vec val -> gsub vec (vsmul val pivotv)) vecs vals
