module LanguageSpec (spec) where

import Test.Hspec
import Language

spec :: Spec
spec = do
  describe "parse" $ do
    it "does not raise an error" $ do
      let program = parse "f = 3 ;                              \n\
                          \g x y = let z = x in z ;             \n\
                          \h x = case (let y = x in y) of       \n\
                          \        <2> -> 2 ;                   \n\
                          \        <2> -> 5                       "

      program `shouldNotBe` []

  describe "clex" $ do
    it "handles multicharacter tokens" $ do
      let tokens = clex "== ~= >= <= ->"

      tokens `shouldBe` ["==", "~=", ">=", "<=", "->"]

  describe "pExpr" $ do
    context "operator precedence and associativity" $ do
      it "|" $ do
        let parsed = head $ pExpr $ clex "a | b"

        parsed `shouldBe` ((EAp (EAp (EVar "|") (EVar "a")) (EVar "b")), [])

      it "&" $ do
        let parsed = head $ pExpr $ clex "a & b"

        parsed `shouldBe` ((EAp (EAp (EVar "&") (EVar "a")) (EVar "b")), [])

      it "<" $ do
        let parsed = head $ pExpr $ clex "a < b"

        parsed `shouldBe` ((EAp (EAp (EVar "<") (EVar "a")) (EVar "b")), [])

      it "+" $ do
        let parsed = head $ pExpr $ clex "a + b"

        parsed `shouldBe` ((EAp (EAp (EVar "+") (EVar "a")) (EVar "b")), [])

      it "-" $ do
        let parsed = head $ pExpr $ clex "a - b"

        parsed `shouldBe` ((EAp (EAp (EVar "-") (EVar "a")) (EVar "b")), [])

      it "*" $ do
        let parsed = head $ pExpr $ clex "a * b"

        parsed `shouldBe` ((EAp (EAp (EVar "*") (EVar "a")) (EVar "b")), [])

      it "/" $ do
        let parsed = head $ pExpr $ clex "a / b"

        parsed `shouldBe` ((EAp (EAp (EVar "/") (EVar "a")) (EVar "b")), [])

      it "associates correctly" $ do
        let parsed1 = head $ pExpr $ clex "5 + 10 / 5"
            parsed2 = head $ pExpr $ clex "5 + (10 / 5)"

        parsed1 `shouldBe` parsed2
