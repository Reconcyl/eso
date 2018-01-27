module Base (Instruction (..), Expression (..), Program) where

import Data.Vector

-- The base Instruction, Expression, and Program types.

data Instruction = Label String Expression
                 | Assign Expression Expression
                 | DebugTape
                 | Breakpoint
                 | DebugPrintExpr Expression
                 deriving (Show)

data Expression = Add Expression Expression
                | Negate Expression
                | Deref Expression
                | Normalize Expression
                | Literal Integer
                | DebugWrite Expression
                | Hole (Maybe String)
                deriving (Show)

type Program = Vector Instruction