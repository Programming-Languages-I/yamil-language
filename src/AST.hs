module AST
        ( module AST
        , ArithmeticOperator (..)
        , ComparisonOperator (..)
        , ConditionExpr (..)
        , Expr (..)
        , Function (..)
        , FunctionBody (..)
        , LambdaExpr (..)
        , LetStatement (..)
        , Literal (..)
        , OtherwiseMatch (..)
        , Pattern (..)
        , PatternMatch (..)
        , PatternMatches (..)
        , Program (..)
        , ProgramElement (..)
        , ThenExpr (..)
        , Type (..)
        , TypedIdentifier (..)
        , Value (..)
        ) where

-- Identifier and literals
type Identifier = String

data Literal
        = IntLiteral Int
        | BoolLiteral Bool
        | DoubleLiteral Double
        | StringLiteral String
        deriving (Eq, Show)

-- Data types
data Type
        = TInt
        | TBool
        | TDouble
        | TString
        deriving (Eq, Show)

-- Expressions
data Expr
        = FunctionCall Identifier [Value]
        | IfExpr ConditionExpr ThenExpr ThenExpr
        | BinaryExpr Value ArithmeticOperator Value
        | ValueExpr Value
        deriving (Eq, Show)

data LambdaExpr
        = LambdaExpr [TypedIdentifier] Expr
        deriving (Eq, Show)

data Value
        = VLiteral Literal
        | VIdentifier Identifier
        | VFunctionCall Identifier [Value]
        deriving (Eq, Show)

data ConditionExpr
        = Condition Value ComparisonOperator Value
        | ConditionAnd ConditionExpr ConditionExpr
        | ConditionOr ConditionExpr ConditionExpr
        | ConditionBool Bool
        deriving (Eq, Show)

data ThenExpr
        = ThenMainExpr Expr
        | ThenLiteral Literal
        | ThenIdentifier Identifier
        deriving (Eq, Show)

-- Parameters and types
data TypedIdentifier
        = TypedIdentifier Identifier Type
        deriving (Eq, Show)

-- Operators
data ArithmeticOperator
        = Add
        | Subtract
        | Multiply
        | Divide
        deriving (Eq, Show)

data ComparisonOperator
        = LessThan
        | GreaterThan
        | LessEqual
        | GreaterEqual
        | Equal
        | NotEqual
        deriving (Eq, Show)

-- Pattern matching
data Pattern
        = PLiteral Literal
        | PIdentifier Identifier
        deriving (Eq, Show)

data PatternMatch
        = PatternMatchExp Pattern Expr
        | PatternMatchLit Pattern Literal
        deriving (Show, Eq)

data OtherwiseMatch
        = OtherwiseExp Expr
        | OtherwiseLit Literal
        deriving (Show, Eq)

data PatternMatches
        = FullPatternMatch [PatternMatch] OtherwiseMatch
        deriving (Show, Eq)                

-- Functions
data Function
        = Function Identifier [TypedIdentifier] Type FunctionBody
        deriving (Eq, Show)

data FunctionBody
        = FBody [FunctionBodyOpts]
        | FBPatternMatch PatternMatches
        | FBLambdaExpr LambdaExpr
        deriving (Eq, Show)

data FunctionBodyOpts
        = FBExpr Expr
        | FBLetStatement LetStatement
        | FBEmpty
        deriving (Eq, Show)

-- Let Statement
data LetStatement
        = LetStatement TypedIdentifier Expr
        deriving (Eq, Show)

-- Program (start)
data Program
        = Program [ProgramElement]
        deriving (Eq, Show)

data ProgramElement
        = PEFunction Function
        | PELetStatement LetStatement
        | PEFunctionCall Expr
        deriving (Eq, Show)
