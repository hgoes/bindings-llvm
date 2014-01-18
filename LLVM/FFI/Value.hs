{-# OPTIONS -cpp -pgmPcpphs -optP--cpp #-}
module LLVM.FFI.Value
       (Value(),ValueC(),
        Argument(),
        argumentGetParent,
        argumentGetArgNo,
        InlineAsm(),
        MDNode(),
        mdNodeGetNumOperands,
        mdNodeGetOperand,
        MDString(),
        PseudoSourceValue(),
        FixedStackPseudoSourceValue(),
        GetType(..),
        deleteValue,
        valueDump,
        valueGetType,
        hasName,
        getName,
        getNameString,
        valueUseBegin,
        valueUseEnd,
        Value_use_iterator(),
        ValueUseIteratorC(..))
        where

import LLVM.FFI.Interface
import LLVM.FFI.Type
import LLVM.FFI.OOP
import LLVM.FFI.StringRef
import LLVM.FFI.IPList
import LLVM.FFI.CPP
import LLVM.FFI.SmallVector

import Foreign
import Foreign.C

argumentGetArgNo :: Ptr Argument -> IO Integer
argumentGetArgNo ptr = fmap toInteger (argumentGetArgNo_ ptr)

#include "Helper.h"

TYPE(Value)
TYPE_LEAF(Argument)
SUBTYPE(Value,Argument)
SPECIALIZE_IPLIST(Argument,capi)
TYPE_LEAF(InlineAsm)
SUBTYPE(Value,InlineAsm)
TYPE_LEAF(MDNode)
SUBTYPE(Value,MDNode)
TYPE_LEAF(MDString)
SUBTYPE(Value,MDString)
TYPE(PseudoSourceValue)
SUBTYPE(Value,PseudoSourceValue)
TYPE_LEAF(FixedStackPseudoSourceValue)
SUBTYPE2(Value,PseudoSourceValue,FixedStackPseudoSourceValue)

class GetType value where
  type TypeOfValue value
  getType :: Ptr value -> IO (Ptr (TypeOfValue value))

GETTYPE(Value)
GETTYPE(Argument)
GETTYPE(InlineAsm)
GETTYPE(MDNode)
GETTYPE(MDString)
GETTYPE(PseudoSourceValue)
GETTYPE(FixedStackPseudoSourceValue)

deleteValue :: ValueC t => Ptr t -> IO ()
deleteValue = deleteValue_

valueDump :: ValueC t => Ptr t -> IO ()
valueDump = valueDump_

valueGetType :: ValueC t => Ptr t -> IO (Ptr Type)
valueGetType = valueGetType_

hasName :: ValueC t => Ptr t -> IO Bool
hasName = hasName_

getName :: ValueC t => Ptr t -> IO (Ptr StringRef)
getName = getName_

getNameString :: ValueC t => Ptr t -> IO String
getNameString ptr = do
  str <- getName ptr
  res <- stringRefData str
  deleteStringRef str
  return res

valueUseBegin :: ValueC t => Ptr t -> IO (Ptr (Value_use_iterator User))
valueUseBegin = valueUseBegin_

valueUseEnd :: ValueC t => Ptr t -> IO (Ptr (Value_use_iterator User))
valueUseEnd = valueUseEnd_

instance PairC CUInt (Ptr MDNode) where
  pairSize _ = sizeofPairUnsigned_MDNode
  pairFirst = pairFirstUnsigned_MDNode
  pairSecond = pairSecondUnsigned_MDNode

instance SmallVectorC (Pair CUInt (Ptr MDNode)) where
  newSmallVector = newSmallVectorMDNodePair
  deleteSmallVector = deleteSmallVectorMDNodePair
  smallVectorSize = smallVectorSizeMDNodePair
  smallVectorData = smallVectorDataMDNodePair

class ValueUseIteratorC t where
  valueUseIteratorDeref :: Ptr (Value_use_iterator t) -> IO (Ptr t)
  valueUseIteratorEq :: Ptr (Value_use_iterator t) -> Ptr (Value_use_iterator t) -> IO Bool
  valueUseIteratorNEq :: Ptr (Value_use_iterator t) -> Ptr (Value_use_iterator t) -> IO Bool
  valueUseIteratorAtEnd :: Ptr (Value_use_iterator t) -> IO Bool
  valueUseIteratorNext :: Ptr (Value_use_iterator t) -> IO (Ptr (Value_use_iterator t))
  valueUseIteratorGetUse :: Ptr (Value_use_iterator t) -> IO (Ptr Use)
  valueUseIteratorGetOperandNo :: Ptr (Value_use_iterator t) -> IO CUInt

instance ValueUseIteratorC User where
  valueUseIteratorDeref = valueUseIteratorUserDeref
  valueUseIteratorEq = valueUseIteratorUserEq
  valueUseIteratorNEq = valueUseIteratorUserNEq
  valueUseIteratorAtEnd = valueUseIteratorUserAtEnd
  valueUseIteratorNext = valueUseIteratorUserNext
  valueUseIteratorGetUse = valueUseIteratorUserGetUse
  valueUseIteratorGetOperandNo = valueUseIteratorUserGetOperandNo
