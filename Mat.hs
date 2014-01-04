module Mat
  where



class MatGroup g where
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

instance (Num a) => MatGroup (Gr a) where
  dim g = 0
  gzero n = Gr 0
  gadd (Gr a) (Gr b) = Gr (a + b)
  gneg (Gr a) = Gr (-a)

instance (Fractional a) => MatField (Gr a) where
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

vsmul :: (MatField f) => f -> Vector f -> Vector f
vsmul a (Vec v)  = Vec (map (fmul a) v)

vimul :: (MatField f) => Vector f -> Vector f -> f
vimul (Vec v0) (Vec v1)  = foldl gadd (gzero 1) (zipWith fmul v0 v1)

velem :: Vector f -> Int -> f
velem (Vec v) n = v !! n

data Matrix f = Mat [(Vector f)]
  deriving (Eq, Read, Show)

instance (MatGroup f) => MatGroup (Matrix f) where
  dim (Mat m) = length m
  gzero n = Mat (take n $ repeat (gzero n))
  gadd (Mat m0) (Mat m1) = Mat (zipWith gadd m0 m1)
  gneg (Mat m) = Mat (map gneg m)

mvmul :: (MatField f) => Matrix f -> Vector f -> Vector f
mvmul (Mat m) v = Vec (map (vimul v) m)

melem :: Matrix f -> Int -> Int -> f
melem (Mat m) row col = velem (m !! row) col

mtrans :: Matrix f -> Matrix f
mtrans (Mat m) = Mat [Vec [velem v ix | v <- m] | ix <- [0 .. length m - 1]]

mmul :: (MatField f) => Matrix f -> Matrix f -> Matrix f
mmul (Mat m0) (Mat m1) = Mat m0

-- instance (MatField f) => MatField (Matrix f)
