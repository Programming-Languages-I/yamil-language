module AST
  ( Identifier,
    Literal (..),
    Type (..),
    Expr (..),
    Value (..),
    ConditionExpr (..),
    ThenExpr (..),
    TypedIdentifier (..),
    ArithmeticOperator (..),
    ComparisonOperator (..),
    Pattern (..),
    PatternMatch (..),
    Function (..),
    FunctionBody (..),
    LetStatement (..),
    Program (..),
    ProgramElement (..),
  )
where

-- Identifier and literals
type Identifier = String

data Literal
    = IntLiteral Int
    | BoolLiteral Bool
    | DoubleLiteral Double
    | StringLiteral String
    deriving (Show, Eq)

-- Data types
data Type
    = TInt
    | TBool
    | TDouble
    | TString
    deriving (Show, Eq)

-- Expressions
data Expr
    = FunctionCall Identifier [Value]
    | IfExpr ConditionExpr ThenExpr ThenExpr
    | BinaryExpr Value ArithmeticOperator Value
    | LambdaExpr [TypedIdentifier] Expr
    deriving (Show, Eq)

data Value
    = VLiteral Literal
    | VIdentifier Identifier
    | VFunctionCall Identifier [Value]
    deriving (Show, Eq)

data ConditionExpr
    = Condition Value ComparisonOperator Value
    | ConditionAnd ConditionExpr ConditionExpr
    | ConditionOr ConditionExpr ConditionExpr
    | ConditionBool Bool
    deriving (Show, Eq)

data ThenExpr 
    = ThenMainExpr Expr Expr
    | ThenLiteral Literal
    | ThenIdentifier Identifier
    deriving(Show, Eq)

-- Parameters and types
data TypedIdentifier = TypedIdentifier Identifier Type
    deriving (Show, Eq)

-- Operators
data ArithmeticOperator
    = Add | Subtract | Multiply | Divide
    deriving (Show, Eq)

data ComparisonOperator
    = LessThan | GreaterThan | LessEqual | GreaterEqual | Equal | NotEqual
    deriving (Show, Eq)

-- Pattern matching
data Pattern
    = PLiteral Literal
    | PIdentifier Identifier
    deriving (Show, Eq)

data PatternMatch
    = PatternMatch Pattern Expr
    | Otherwise Expr
    deriving (Show, Eq)

-- Functions
data Function = Function Identifier [TypedIdentifier] Type FunctionBody
    deriving (Show, Eq)

data FunctionBody
    = FBExpr Expr
    | FBPatternMatch [PatternMatch]
    | FBLetStatement LetStatement
    deriving (Show, Eq)

-- Let Statement
data LetStatement = LetStatement TypedIdentifier Expr
    deriving (Show, Eq)

-- Program (start)
data Program = Program [ProgramElement]
    deriving (Show, Eq)

data ProgramElement
    = PEFunction Function
    | PELetStatement LetStatement
    | PEFunctionCall Expr
    deriving (Show, Eq)
