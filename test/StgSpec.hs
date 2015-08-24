module StgSpec (spec) where

import Test.Hspec
import Language
import STG
import qualified Utils as U

spec :: Spec
spec = do
  describe "pprProgram" $ do
    it "pretty prints the program" $ do
      let program = [ ("main", ([], NonUpdateable, [], App "f" [VarArg "y", LitArg 10])) ]
          printed = iDisplay $ pprStgProgram program
      printed `shouldBe` "main = {} n {} ->\n\
                         \  f {y 10#}"

  describe "pprStgExpr" $ do
    it "pretty prints lets" $ do
      let expr = (Let True [("a", ([], NonUpdateable, [], Literal 1))] (App "a" []))
          printed = iDisplay $ pprStgExpr expr
      printed `shouldBe` "letrec\n\
                         \  a = {} n {} ->\n\
                         \    1#\n\
                         \in a {}"

    it "pretty prints cases" $ do
      let expr = (Case (App "xs" []) [(AlgAlt "Nil" [] (ConApp "Nil" []))])
          printed = iDisplay $ pprStgExpr expr

      printed `shouldBe` "case xs {} of\n\
                         \  Nil {} -> Nil {}"
