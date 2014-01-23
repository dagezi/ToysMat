module MatSpec where

import Toys.Mat.Field
import Toys.Mat.Vector
import Toys.Mat.Mat

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding ((.&.))

zero4 :: Matrix (Gr Double)
zero4 = gzero 4

one4 :: Matrix (Gr Double)
one4 = mone 4

spec :: Spec
spec = do
     describe "mmul" $ do
       it "one `mmul` zero" $
         (one4 `mmul` zero4) `shouldBe` zero4


