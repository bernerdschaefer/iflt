module LanguageSpec (spec) where

import Test.Hspec
import Language

spec :: Spec
spec = do
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
