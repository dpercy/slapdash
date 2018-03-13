
import Test.Hspec

import Core
import qualified Parse as P

main :: IO ()
main = hspec $ do
  describe "lex" $ do
    it "lexes things" $ do
      P.lex ")=(fgs2 42 -0 " `shouldBe`
        Right [P.Close, P.Equals, P.Open, P.Id "fgs2", P.Integer "42", P.Integer "-0"]
  describe "parse" $ do
    it "parses expressions" $ do
      P.parse "1" `shouldBe` Right (Program [Expr (Num 1)])
      P.parse "x" `shouldBe` Right (Program [Expr (Var "x")])
      P.parse "f x 1 y" `shouldBe` Right (Program [Expr (App (App (App (Var "f")
                                                                   (Var "x"))
                                                              (Num 1))
                                                         (Var "y"))])
    it "parses equations" $ do
      P.parse "1 = 2" `shouldBe` Right (Program [Rule (Num 1, Num 2)])
      P.parse "1 = 2" `shouldBe` Right (Program [Rule (Num 1, Num 2)])
        
