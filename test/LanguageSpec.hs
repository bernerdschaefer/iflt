module LanguageSpec (spec) where

import Test.Hspec
import Language

spec :: Spec
spec = do
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
