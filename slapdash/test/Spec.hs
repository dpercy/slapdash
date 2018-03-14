
import Test.Hspec

import Core
import qualified Parse as P

main :: IO ()
main = hspec $ do
  describe "lex" $ do
    it "lexes things" $ do
      P.lex ")=(fgs2 42 -0 " `shouldBe`
        Right [P.Close, P.Equals, P.Open, P.Id "fgs2", P.Integer "42", P.Integer "-0"]
      P.lex "1;2" `shouldBe` Right [P.Integer "1", P.Semicolon, P.Integer "2"]
  describe "parse" $ do
    it "parses expressions" $ do
      P.parse "1" `shouldBe` Right (Program [Expr (Num 1)])
      P.parse "x" `shouldBe` Right (Program [Expr (Var "x")])
      P.parse "f x 1 y" `shouldBe` Right (Program [Expr (App (App (App (Var "f")
                                                                   (Var "x"))
                                                              (Num 1))
                                                         (Var "y"))])
      P.parse "f (g x)" `shouldBe` Right (Program [Expr (App (Var "f") (App (Var "g") (Var "x")))])
    it "parses equations" $ do
      P.parse "1 = 2" `shouldBe` Right (Program [Rule (Num 1, Num 2)])
      P.parse "1 = 2" `shouldBe` Right (Program [Rule (Num 1, Num 2)])
    it "parses multiple statements" $ do
      P.parse "1;2" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])
      P.parse "1;;2" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])
      P.parse "1\n2" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])
      P.parse "1\n\n2\n" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])

