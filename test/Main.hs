module Main (main) where

import Befunge93
import System.Random (newStdGen)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Interpret" $ do
    it "should display hello world" $ do
      g <- newStdGen
      interpret g ">              v\nv\"Hello World!\"<\n>:v             \n^,_@" `shouldBe` "Hello World!"

    it "should calculate factorial" $ do
      g <- newStdGen
      interpret g "8>:1-:v v *_$.@\n ^    _$>\\:^" `shouldBe` "40320"

    it "quine" $ do
      g <- newStdGen
      interpret g "01->1# +# :# 0# g# ,# :# 5# 8# *# 4# +# -# _@" `shouldBe` "01->1# +# :# 0# g# ,# :# 5# 8# *# 4# +# -# _@"
