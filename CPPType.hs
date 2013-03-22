module CPPType where

import Text.ParserCombinators.ReadP
import Data.Char

type NS = [String]

data Type = Type [TypeQualifier] TypeC deriving (Eq,Ord,Show)

data TypeQualifier = QConst deriving (Eq,Ord,Show)

data TypeC = NamedType NS String [Type]
           | PtrType TypeC
           | RefType TypeC
           deriving (Eq,Ord,Show)

data FunSig = FunSig Type NS String [Type] deriving (Eq,Ord,Show)
