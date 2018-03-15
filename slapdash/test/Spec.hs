
import Test.Hspec

import Core
import qualified Parse as P
import qualified Rewrite as R
import qualified Prim

main :: IO ()
main = hspec $ do
  describe "lex" $ do
    it "lexes things" $ do
      P.lex ")=(fgs2 42 -0 " `shouldBe`
        Right [P.Close, P.Equals, P.Open, P.Id "fgs2", P.Integer "42", P.Integer "-0"]
      P.lex "1;2" `shouldBe` Right [P.Integer "1", P.Semicolon, P.Integer "2"]
      P.lex "1;if 2" `shouldBe` Right [P.Integer "1", P.Semicolon, P.If, P.Integer "2"]
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
      P.parse "1 = 2" `shouldBe` Right (Program [Eqn (Num 1, Num 2, Nothing)])
      P.parse "1 = 2 if 3" `shouldBe` Right (Program [Eqn (Num 1, Num 2, Just (Num 3))])
    it "parses multiple statements" $ do
      P.parse "1;2" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])
      P.parse "1;;2" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])
      P.parse "1\n2" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])
      P.parse "1\n\n2\n" `shouldBe` Right (Program [Expr (Num 1), Expr (Num 2)])
    it "parses strings" $ do
      P.parse "  \"hi\" " `shouldBe` Right (Program [Expr (Str "hi")])
      P.parse ("\"" ++ "\\\\" ++ "\"") `shouldBe` Right (Program [Expr (Str "\\")])
      P.parse ("\"" ++ "\\t" ++ "\"") `shouldBe` Right (Program [Expr (Str "\t")])
      
  describe "unparse" $ do
    it "unparses" $ do
      P.unparse (Program [Eqn (Num 1, Num 2, Just (Num 3))])
        `shouldBe` "1 = 2 if 3\n"
      P.unparse (Program [Expr (App (App (Var "f") (Var "x")) (Var "y"))])
        `shouldBe` "f x y\n"
      P.unparse (Program [Expr (App (App (Var "f") (App (Var "g") (Var "x"))) (Var "y"))])
        `shouldBe` "f (g x) y\n"
  describe "eval" $ do
    it "does global variables" $ do
      let rule = R.interpEquations [
            (Var "x", Var "y", Nothing)
            , (Var "y", Var "z", Nothing)
            ]
      R.eval rule (Var "y") `shouldBe` Var "z"
      R.eval rule (Var "x") `shouldBe` Var "z"
      R.eval rule (Var "a") `shouldBe` Var "a"
      R.eval rule (Num 23) `shouldBe` Num 23
    it "does simple calls" $ do
      let rule = R.interpEquations [
            (App (Var "square") (Var "n"),
             App (App (Var "mul") (Var "n")) (Var "n"),
             Nothing)
            , (App (Var "inc") (Var "x"),
               App (App (Var "sub") (Var "n")) (Num 1),
               Nothing)
            ]
      R.eval rule (Var "square") `shouldBe` Var "square"
      R.eval rule (App (Var "square") (Var "v"))
        `shouldBe` App (App (Var "mul") (Var "v")) (Var "v")
      R.eval rule (App (Var "square") (Var "mul"))
        `shouldBe` App (App (Var "mul") (Var "mul")) (Var "mul")
      R.eval rule (App (Var "square") (Var "square"))
        `shouldBe` App (App (Var "mul") (Var "square")) (Var "square")
    it "does conditional rules" $ do
      let rule = R.interpEquations [
            (App (Var "even") (Num 2), Var "true", Nothing)
            , (App (Var "even") (Num 4), Var "true", Nothing)
            , (App (Var "even") (Var "x"), Var "false", Nothing)
            , (App (Var "f") (Var "n"), Num 1,
               Just (App (Var "even") (Var "n")))
            , (App (Var "f") (Var "n"), Num 0, Nothing)
            ]
      R.eval rule (App (Var "f") (Num 2)) `shouldBe` Num 1
      R.eval rule (App (Var "f") (Num 4)) `shouldBe` Num 1
      R.eval rule (App (Var "f") (Num 3)) `shouldBe` Num 0
      R.eval rule (App (Var "f") (Var "derp")) `shouldBe` Num 0
    it "does arithmetic" $ do
      let rule = R.alts [ Prim.rAdd
                        , Prim.rMul
                        ]
      R.eval rule (App (App (Var "add") (Num 1)) (Num 2)) `shouldBe` Num 3
      R.eval rule (App (App (Var "mul") (Num 3)) (Num 2)) `shouldBe` Num 6
      R.eval rule (App (App (Var "mul") (Var "foo")) (Var "foo"))
        `shouldBe` App (App (Var "mul") (Var "foo")) (Var "foo")
    it "does string ops" $ do
      let rule = R.alts [ Prim.rStringAdd ]
      R.eval rule (App (App (Var "add") (Str "x")) (Str "y"))
        `shouldBe` Str "xy"
                        
