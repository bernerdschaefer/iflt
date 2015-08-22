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

    it "handles conditions" $ do
      let
        program = "factorial n = if n 1 (n * factorial (n - 1)) ; \n\
                  \main = factorial 3                               "
        states = eval $ compile $ parse program
        (instr, fptr, stack, vstack, dump, heap, cstore, stats) = last states

      (head vstack) `shouldBe` 6
      let steps = statGetSteps stats
      steps `shouldSatisfy` (< 288) -- steps before if inlining

    it "implements efficient arithmetic" $ do
      let
        program = "f x y z = (x + y) * z ; \n\
                  \g = 1 ;                 \n\
                  \main = f g 2 3            "
        states = eval $ compile $ parse program
        (instr, fptr, stack, vstack, dump, heap, cstore, stats) = last states
      (head vstack) `shouldBe` 9
      let steps = statGetSteps stats
      steps `shouldSatisfy` (< 32) -- 33 steps before B compilation

    it "implements efficient comparisons" $ do
      let
        program = "fib n = if (n < 2) 1 (fib (n - 1) + fib (n - 2)) ; \n\
                  \main = fib 6                                         "
        states = eval $ compile $ parse program
        (instr, fptr, stack, vstack, dump, heap, cstore, stats) = last states
      (head vstack) `shouldBe` 8
      let steps = statGetSteps stats
      steps `shouldSatisfy` (< 777) -- steps before inlining

    it "supports basic let" $ do
      let
        program = "f x = let y = f 3 in g x y ; \n\
                  \main = f 5                     "
        states = eval $ compile $ parse program
        (instr, fptr, stack, vstack, dump, heap, cstore, stats) = last states
      (head vstack) `shouldBe` 8
