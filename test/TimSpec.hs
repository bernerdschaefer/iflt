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

      (head vstack) `shouldBe` 1

    it "handles simple arithmetic" $ do
      let
        program = "four = two * two ;   \n\
                  \two  = 10 / five ;   \n\
                  \five = 6 - 2 ;       \n\
                  \main = four + four     "
        states = eval $ compile $ parse program
        (instr, fptr, stack, vstack, dump, heap, cstore, stats) = last states

      (head vstack) `shouldBe` 8
