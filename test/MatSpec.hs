module MatSpec where

import Toys.Mat.Field
import Toys.Mat.Mat

import Test.Hspec

zero4 :: Matrix (Gr Double)
zero4 = gzero 4

one4 :: Matrix (Gr Double)
one4 = mone 4

two4 :: Matrix (Gr Double)
two4 = msmul (Gr 2.0) one4

spec :: Spec
spec = do
     describe "gadd for Matrix" $ do
       it "one `gadd` one = two" $
         (one4 `gadd` one4) `shouldBe` two4
       it "one `gadd` zero = one" $
         (one4 `gadd` zero4) `shouldBe` one4

     describe "gsub for Matrix" $ do
       it "one `gsub` one = zero" $
         (one4 `gsub` one4) `shouldBe` zero4
       it "two `gsub` one = one" $
         (two4 `gsub` one4) `shouldBe` one4

     describe "mmul" $ do
       it "one `mmul` zero = zero" $
         (one4 `mmul` zero4) `shouldBe` zero4
       it "one `mmul` one = one" $
         (one4 `mmul` one4) `shouldBe` one4
