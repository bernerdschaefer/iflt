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

  describe "transformCoreExpr" $ do
    it "handles applications" $ do
      let expr = (EAp (EAp (EAp (EAp (EVar "g") (ENum 9)) (ENum 1)) (ENum 2)) (ENum 3))
          (transformed, _) = transformCoreExpr expr
      putStrLn $ iDisplay $ pprStgExpr transformed

    it "handles cases" $ do
      let (_, _, expr) = (head program)
          transformed = transformCoreProgram program
          program = parse "map f xs = case xs of\n\
                          \             <1>      -> Pack{1,0};\n\
                          \             <2> y ys -> Pack{2,2} (f y) (map f ys)"

      print expr
      putStrLn $ iDisplay $ pprExpr expr
      putStrLn $ iDisplay $ pprStgProgram transformed

    it "handles free variables" $ do
      let expr = (ELet nonRecursive [("fy", (EAp (EVar "f") (EVar "y")))] (EVar "fy"))
          (stgExpr, _) = transformCoreExpr expr
          (Let _ (bind:_) _) = stgExpr
          (_, (freeVars, _, _, _)) = bind

      freeVars `shouldBe` ["f", "y"]

  describe "eval" $ do
    it "works for simple cases" $ do
      let transformed = transformCoreProgram program
          program = parse "main = 1"
          compiled = compileStgProgram transformed initialState
          state = last (eval compiled)

      (code state) `shouldBe` (ReturnInt 1)

    it "handles application" $ do
      let transformed = transformCoreProgram program
          program = parse "f x = x ; main = f 2"
          compiled = compileStgProgram transformed initialState
          state = last (eval compiled)

      (code state) `shouldBe` (ReturnInt 2)

    it "handles let" $ do
      let transformed = transformCoreProgram $ parse program
          program = "f x = let y = 3 in g y ;     \n\
                    \g x = x ;                    \n\
                    \main = f 5                     "
          compiled = compileStgProgram transformed initialState
          state = last (eval compiled)

      (code state) `shouldBe` (ReturnInt 3)
