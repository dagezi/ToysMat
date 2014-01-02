module Mat
  where

class Group g where
  gzero :: g
  gadd, gsub :: g -> g -> g
  gneg :: g -> g
  gsub a b  = gadd a (gneg b)

class Group f => Field f where
  fone :: f
  fmul, fdiv :: f -> f -> f
  finv :: f -> f
  fdiv a b  = fmul a (finv b)

data Gr a = Gr a
  deriving (Eq, Ord, Read, Show)

instance (Num a) => Group (Gr a) where
  gzero = Gr 0
  gadd (Gr a) (Gr b) = Gr (a + b)
  gneg (Gr a) = Gr (-a)

instance (Fractional a) => Field (Gr a) where
  fone = Gr 1
  fmul (Gr a) (Gr b) = Gr (a * b)
  finv (Gr a) = Gr (1 / a)

data Vector f = Vec [f]
  deriving (Eq, Read, Show)

instance (Group f) => Group (Vector f) where
  gzero = Vec (repeat gzero)
  gadd (Vec a) (Vec b) = Vec (zipWith gadd a b)
  gneg (Vec a) = Vec (map gneg a)

vsmul :: (Field f) => f -> Vector f -> Vector f
vsmul a (Vec v)  = Vec (map (fmul a) v)

vimul :: (Field f) => Vector f -> Vector f -> f
vimul (Vec v0) (Vec v1)  = foldl gadd gzero (zipWith fmul v0 v1)


data Matrix f = Mat [(Vector f)]
  deriving (Eq, Read, Show)

instance (Group f) => Group (Matrix f) where
  gzero = Mat (repeat gzero)
  gadd (Mat m0) (Mat m1) = Mat (zipWith gadd m0 m1)
  gneg (Mat m) = Mat (map gneg m)

mvmul :: (Field f) => Matrix f -> Vector f -> Vector f
mvmul (Mat m) v = Vec (map (vimul v) m)

mmul :: (Field f) => Matrix f -> Matrix f -> Matrix f
mmul m0 m1 = gzero


