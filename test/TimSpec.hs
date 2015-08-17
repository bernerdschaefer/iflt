module TimSpec (spec) where

import Test.Hspec
import Language
import TIM

spec :: Spec
spec = do
  describe "eval" $ do
    it "handles simple identity" $ do
      let states = eval $ compile $ parse "f = I ; main = f 1"
          (instr, fptr, stack, vstack, dump, heap, cstore, stats) = last states

      instr `shouldBe` []
      fptr `shouldBe` (FrameInt 1)
