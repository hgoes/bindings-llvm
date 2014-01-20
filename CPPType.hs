module CPPType where

import Text.ParserCombinators.ReadP
import Data.Char

data ClassName = ClassName { className :: String
                           , classArgs :: [Type]
                           }
               deriving (Show,Eq,Ord)

type NS = [ClassName]

data Type = Type { qualifiers :: [TypeQualifier]
                 , typeDesc :: TypeC }
          | TypeInt Integer
          deriving (Eq,Ord,Show)

data TypeQualifier = QConst deriving (Eq,Ord,Show)

data TypeC = NamedType NS String [Type] Bool
           | EnumType NS String
           | PtrType TypeC
           | RefType TypeC
           deriving (Eq,Ord,Show)

data FunSig = FunSig Type NS String [Type] deriving (Eq,Ord,Show)
