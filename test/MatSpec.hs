module MatSpec where

import Toys.Mat.Field
import Toys.Mat.Vector
import Toys.Mat.Mat

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

instance Arbitrary f => Arbitrary (Gr f) where
  arbitrary = do
    x <- arbitrary
    return $ Gr x

-- Supports only 4 dimension vector
instance Arbitrary f => Arbitrary (Vector f) where
  arbitrary = do
    xs <- sequence [arbitrary, arbitrary, arbitrary, arbitrary]
    return $ Vec xs

-- Supports only 4 dimension matrix
instance Arbitrary f => Arbitrary (Matrix f) where
  arbitrary = do
    vectors <- sequence [arbitrary, arbitrary, arbitrary, arbitrary]
    return $ Mat vectors

vec0_4 :: Vector (Gr Rational)
vec0_4 = gzero 4

mat0_4 :: Matrix (Gr Rational)
mat0_4 = gzero 4

mat1_4 :: Matrix (Gr Rational)
mat1_4 = mone 4

mat2_4 :: Matrix (Gr Rational)
mat2_4 = msmul (Gr 2) mat1_4

spec :: Spec
spec = do
     describe "gadd for Matrix" $ do
       it "one `gadd` one = two" $
         (mat1_4 `gadd` mat1_4) `shouldBe` mat2_4
       it "one `gadd` zero = one" $
         (mat1_4 `gadd` mat0_4) `shouldBe` mat1_4

     describe "gsub for Matrix" $ do
       it "one `gsub` one = zero" $
         (mat1_4 `gsub` mat1_4) `shouldBe` mat0_4
       it "two `gsub` one = one" $
         (mat2_4 `gsub` mat1_4) `shouldBe` mat1_4

     describe "mvmul" $ do
       prop "zero `mvmul` any vector == zero vector" $ \ v_4 ->
         (mat0_4 `mvmul` v_4) == vec0_4
       prop "one `mvmul` any vector == same vector" $ \v_4 ->
         (mat1_4 `mvmul` v_4) == v_4

     describe "mmul" $ do
       it "one `mmul` zero = zero" $
         (mat1_4 `mmul` mat0_4) `shouldBe` mat0_4
       it "one `mmul` one = one" $
         (mat1_4 `mmul` mat1_4) `shouldBe` mat1_4

       prop "zero `mmul` arbitrary == zero" $ \m_4 ->
         mat0_4 `mmul` m_4 == mat0_4
       prop "one `mmul` arbitrary == same matrix" $ \m_4 ->
         mat1_4 `mmul` m_4 == m_4

     describe "minv" $ do
       it "minv one == one" $
         (minv mat1_4) `shouldBe` mat1_4
       it "(minv two) `mmul` two == one" $
         ((minv mat2_4) `mmul` mat2_4) `shouldBe` mat1_4

       prop "(minv any mat) `mmul` same mat == one or invertible" $ \m_4 ->
         mdet m_4 == gzero 1 || (minv m_4) `mmul` m_4 == mat1_4
