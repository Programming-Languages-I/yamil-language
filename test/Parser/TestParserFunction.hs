module Parser.TestParserFunction
        ( module Parser.TestParserFunction
        ) where

import           AST

import           Parser.ParserFunction

import           Test.Hspec

import           Text.Parsec

testParseLetStatement :: Spec
testParseLetStatement = describe "parseLetStatement" $ do
  it "parses a let statement with sum" $ do
    parse parseLetStatement "" "let sum:int=1+2" `shouldBe` Right (LetStatement (TypedIdentifier "sum" TInt) (BinaryExpr (VLiteral (IntLiteral 1)) Add (VLiteral (IntLiteral 2))))

  it "parses a let statement with no spaces" $ do
    parse parseLetStatement "" "let cba:int=5 * 2" `shouldBe` Right (LetStatement (TypedIdentifier "cba" TInt) (BinaryExpr (VLiteral (IntLiteral 5)) Multiply (VLiteral (IntLiteral 2))))

  it "parses a let statement with Division" $ do
    parse parseLetStatement "" "let x_a:int= 8/2" `shouldBe` Right (LetStatement (TypedIdentifier "x_a" TInt) (BinaryExpr (VLiteral (IntLiteral 8)) Divide (VLiteral (IntLiteral 2))))

  it "parses a let statement with Subtract" $ do
    parse parseLetStatement "" "let x:int = 3 - 4" `shouldBe` Right (LetStatement (TypedIdentifier "x" TInt) (BinaryExpr (VLiteral (IntLiteral 3)) Subtract (VLiteral (IntLiteral 4))))

  it "parses a let statement with a function call" $ do
    parse parseLetStatement "" "let z:int = foo()" `shouldBe` Right (LetStatement (TypedIdentifier "z" TInt) (FunctionCall "foo" []))

testParseFunctionType :: Spec
testParseFunctionType = describe "parseFunctionType" $ do
  it "parses function type int" $ do
    parse parseFunctionType "" "-> int =" `shouldBe` Right (TInt)

  it "parses function type double" $ do
    parse parseFunctionType "" "-> double =" `shouldBe` Right (TDouble)

  it "parses function type bool" $ do
    parse parseFunctionType "" "-> bool =" `shouldBe` Right (TBool)

  it "parses function type string" $ do
    parse parseFunctionType "" "-> string = " `shouldBe` Right (TString)

testParseFunctionBody :: Spec
testParseFunctionBody = describe "parseFunction" $ do
  it "parses a function body with binary expresion" $ do
    parse parseFunctionBody "" "x + y"
      `shouldBe` Right
        (FBody [FBExpr (BinaryExpr (VIdentifier "x") Add (VIdentifier "y"))])

  it "parses a function body with let stmts and binary expresion" $ do
    parse
      parseFunctionBody
      ""
      "let sum: int = add(a, b)\n\
      \let product: int = a * b\n\
      \sum + product"
      `shouldBe` Right
        ( FBody
            [ FBLetStatement (LetStatement (TypedIdentifier "sum" TInt) (FunctionCall "add" [VIdentifier "a", VIdentifier "b"])),
              FBLetStatement (LetStatement (TypedIdentifier "product" TInt) (BinaryExpr (VIdentifier "a") Multiply (VIdentifier "b"))),
              FBExpr (BinaryExpr (VIdentifier "sum") Add (VIdentifier "product"))
            ]
        )

  it "parses a function body with ifs and let stmts" $ do
    parse
      parseFunctionBody
      ""
      "let y: int = 0\nif y == 0 then if a == 2 then a else b \n else  2+3"
      `shouldBe` Right
        ( FBody
            [ FBLetStatement
                ( LetStatement
                    (TypedIdentifier "y" TInt)
                    (ValueExpr (VLiteral (IntLiteral 0)))
                ),
              FBExpr
                ( IfExpr
                    (Condition (VIdentifier "y") Equal (VLiteral (IntLiteral 0)))
                    ( ThenMainExpr
                        ( IfExpr
                            (Condition (VIdentifier "a") Equal (VLiteral (IntLiteral 2)))
                            (ThenMainExpr (ValueExpr (VIdentifier "a")))
                            (ThenMainExpr (ValueExpr (VIdentifier "b")))
                        )
                    )
                    (ThenMainExpr (BinaryExpr (VLiteral (IntLiteral 2)) Add (VLiteral (IntLiteral 3))))
                )
            ]
        )

  it "parses a function body with lambda" $ do
    parse
      parseFunctionBody
      ""
      "lambda(x: int) -> if x > threshold then True else False"
      `shouldBe` Right
        ( FBLambdaExpr
            ( LambdaExpr
                [TypedIdentifier "x" TInt]
                ( IfExpr
                    (Condition (VIdentifier "x") GreaterThan (VIdentifier "threshold"))
                    (ThenMainExpr (ValueExpr (VLiteral (BoolLiteral True))))
                    (ThenMainExpr (ValueExpr (VLiteral (BoolLiteral False))))
                )
            )
        )

testParseFunction :: Spec
testParseFunction = describe "parseFunction" $ do
  it "parses a function with binary expresion" $ do
    parse parseFunction "" "def add(x: int, y: int) -> int = {\n x+y }"
      `shouldBe` Right
        ( Function
            "add"
            [TypedIdentifier "x" TInt, TypedIdentifier "y" TInt]
            TInt
            (FBody [FBExpr (BinaryExpr (VIdentifier "x") Add (VIdentifier "y"))])
        )

  it "parses a function with binary expresions and function calls" $ do
    parse
      parseFunction
      ""
      "def calculate(a: int, b: int) -> int = { \
      \let sum: int = add(a, b)\n\
      \let product: int = a * b\n\
      \sum + product }"
      `shouldBe` Right
        ( Function
            "calculate"
            [TypedIdentifier "a" TInt, TypedIdentifier "b" TInt]
            TInt
            ( FBody
                [ FBLetStatement (LetStatement (TypedIdentifier "sum" TInt) (FunctionCall "add" [VIdentifier "a", VIdentifier "b"])),
                  FBLetStatement (LetStatement (TypedIdentifier "product" TInt) (BinaryExpr (VIdentifier "a") Multiply (VIdentifier "b"))),
                  FBExpr (BinaryExpr (VIdentifier "sum") Add (VIdentifier "product"))
                ]
            )
        )

  it "parses a function with binary expresions and return function call" $ do
    parse
      parseFunction
      ""
      "def calculate(a: int, b: int) -> int = { \
      \let sum: int = add(a, b)\n\
      \let product: int = a * b\n\
      \add(sum, product) }"
      `shouldBe` Right
        ( Function
            "calculate"
            [TypedIdentifier "a" TInt, TypedIdentifier "b" TInt]
            TInt
            ( FBody
                [ FBLetStatement (LetStatement (TypedIdentifier "sum" TInt) (FunctionCall "add" [VIdentifier "a", VIdentifier "b"])),
                  FBLetStatement (LetStatement (TypedIdentifier "product" TInt) (BinaryExpr (VIdentifier "a") Multiply (VIdentifier "b"))),
                  FBExpr (FunctionCall "add" [VIdentifier "sum", VIdentifier "product"])
                ]
            )
        )

  it "parses a function with ifs and let stmts" $ do
    parse
      parseFunction
      ""
      "def fun(a:int, b:int) ->\
      \int = {\nlet y: int = 0\nif y == 0 then if a == 2 then a else b \n else  2+3\n}"
      `shouldBe` Right
        ( Function
            "fun"
            [TypedIdentifier "a" TInt, TypedIdentifier "b" TInt]
            TInt
            ( FBody
                [ FBLetStatement
                    ( LetStatement
                        (TypedIdentifier "y" TInt)
                        (ValueExpr (VLiteral (IntLiteral 0)))
                    ),
                  FBExpr
                    ( IfExpr
                        (Condition (VIdentifier "y") Equal (VLiteral (IntLiteral 0)))
                        ( ThenMainExpr
                            ( IfExpr
                                (Condition (VIdentifier "a") Equal (VLiteral (IntLiteral 2)))
                                (ThenMainExpr (ValueExpr (VIdentifier "a")))
                                (ThenMainExpr (ValueExpr (VIdentifier "b")))
                            )
                        )
                        (ThenMainExpr (BinaryExpr (VLiteral (IntLiteral 2)) Add (VLiteral (IntLiteral 3))))
                    )
                ]
            )
        )

  it "parses a function with lambda" $ do
    parse
      parseFunction
      ""
      "def lambda() -> bool = {\nlambda(x: int) -> if x > threshold then True else False\n}"
      `shouldBe` Right
        ( Function
            "lambda"
            []
            TBool
            ( FBLambdaExpr
                (LambdaExpr [TypedIdentifier "x" TInt]
                    (IfExpr (Condition (VIdentifier "x") GreaterThan (VIdentifier "threshold"))
                    (ThenMainExpr (ValueExpr (VLiteral (BoolLiteral True))))
                    (ThenMainExpr (ValueExpr (VLiteral (BoolLiteral False)))))
                )
            )
        )

testParserFunctions :: Spec
testParserFunctions = do
  testParseLetStatement
  testParseFunctionType
  testParseFunctionBody
  testParseFunction
