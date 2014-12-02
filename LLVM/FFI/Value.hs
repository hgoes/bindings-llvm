{-# OPTIONS -cpp -pgmPcpphs -optP--cpp #-}
module LLVM.FFI.Value
       (Value(),ValueC(),
        Argument(),
        createArgument,
        argumentGetParent,
        argumentGetArgNo,
        InlineAsm(),
#if HS_LLVM_VERSION >= 302
        AsmDialect(),
        toAsmDialect,
        fromAsmDialect,
        inlineAsmGetDialect,
#endif
        inlineAsmHasSideEffects,
        inlineAsmIsAlignStack,
        inlineAsmGetFunctionType,
        inlineAsmGetAsmString,
        inlineAsmGetConstraintString,
        PseudoSourceValue(),
        FixedStackPseudoSourceValue(),
        GetType(..),
        deleteValue,
        valueDump,
        valueToString,
        valueGetType,
        hasName,
        getName,
        getNameString,
        valueUseBegin,
        valueUseEnd,
#if HS_LLVM_VERSION<305
        Value_use_iterator(),
        ValueUseIteratorC(..)
#else
        Use_iterator(),
        valueUseIteratorDeref,
        valueUseIteratorEq,
        valueUseIteratorNEq,
        valueUseIteratorNext
#endif
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.Type
import LLVM.FFI.OOP
import LLVM.FFI.StringRef
import LLVM.FFI.IPList
import LLVM.FFI.CPP
import LLVM.FFI.SmallVector

import Foreign
import Foreign.C

createArgument :: TypeC tp => Ptr tp -> Ptr Twine -> Ptr Function -> IO (Ptr Argument)
createArgument = createArgument_

argumentGetArgNo :: Ptr Argument -> IO Integer
argumentGetArgNo ptr = fmap toInteger (argumentGetArgNo_ ptr)

#include "Helper.h"

TYPE(Value)
TYPE_LEAF(Argument)
SUBTYPE(Value,Argument)
SPECIALIZE_IPLIST(Argument,capi)
TYPE_LEAF(InlineAsm)
SUBTYPE(Value,InlineAsm)
TYPE(PseudoSourceValue)
#if HS_LLVM_VERSION<305
SUBTYPE(Value,PseudoSourceValue)
TYPE_LEAF(FixedStackPseudoSourceValue)
SUBTYPE2(Value,PseudoSourceValue,FixedStackPseudoSourceValue)
#else
SUBTYPE(PseudoSourceValue,FixedStackPseudoSourceValue)
TYPE_LEAF(FixedStackPseudoSourceValue)
#endif

class GetType value where
  type TypeOfValue value
  getType :: Ptr value -> IO (Ptr (TypeOfValue value))

GETTYPE(Value)
GETTYPE(Argument)
instance GetType InlineAsm where
  type TypeOfValue InlineAsm = PointerType
  getType = inlineAsmGetType
#if HS_LLVM_VERSION<305
GETTYPE(PseudoSourceValue)
GETTYPE(FixedStackPseudoSourceValue)
#endif

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

#if HS_LLVM_VERSION<305
valueUseBegin :: ValueC t => Ptr t -> IO (Ptr (Value_use_iterator User))
#else
valueUseBegin :: ValueC t => Ptr t -> IO (Ptr Use_iterator)
#endif
valueUseBegin = valueUseBegin_

#if HS_LLVM_VERSION<305
valueUseEnd :: ValueC t => Ptr t -> IO (Ptr (Value_use_iterator User))
#else
valueUseEnd :: ValueC t => Ptr t -> IO (Ptr Use_iterator)
#endif
valueUseEnd = valueUseEnd_

foreign import capi "wrapper/extra.h value_to_string"
  valueToString_ :: Ptr a -> IO CString

valueToString :: ValueC t => Ptr t -> IO String
valueToString val = do
  cstr <- valueToString_ val
  hstr <- peekCString cstr
  free cstr
  return hstr

#if HS_LLVM_VERSION<305
class ValueUseIteratorC t where
  valueUseIteratorDeref :: Ptr (Value_use_iterator t) -> IO (Ptr t)
  valueUseIteratorEq :: Ptr (Value_use_iterator t) -> Ptr (Value_use_iterator t) -> IO Bool
  valueUseIteratorNEq :: Ptr (Value_use_iterator t) -> Ptr (Value_use_iterator t) -> IO Bool
  valueUseIteratorNext :: Ptr (Value_use_iterator t) -> IO (Ptr (Value_use_iterator t))
  valueUseIteratorGetUse :: Ptr (Value_use_iterator t) -> IO (Ptr Use)
  valueUseIteratorGetOperandNo :: Ptr (Value_use_iterator t) -> IO CUInt

instance ValueUseIteratorC User where
  valueUseIteratorDeref = valueUseIteratorUserDeref
  valueUseIteratorEq = valueUseIteratorUserEq
  valueUseIteratorNEq = valueUseIteratorUserNEq
  valueUseIteratorNext = valueUseIteratorUserNext
  valueUseIteratorGetUse = valueUseIteratorUserGetUse
  valueUseIteratorGetOperandNo = valueUseIteratorUserGetOperandNo
#else

valueUseIteratorDeref :: Ptr Use_iterator -> IO (Ptr User)
valueUseIteratorDeref = valueUseIteratorUserDeref

valueUseIteratorEq :: Ptr Use_iterator -> Ptr Use_iterator -> IO Bool
valueUseIteratorEq = valueUseIteratorUserEq

valueUseIteratorNEq :: Ptr Use_iterator -> Ptr Use_iterator -> IO Bool
valueUseIteratorNEq = valueUseIteratorUserNEq

valueUseIteratorNext :: Ptr Use_iterator -> IO (Ptr Use_iterator)
valueUseIteratorNext = valueUseIteratorUserNext

#endif

#if HS_LLVM_VERSION >= 302
inlineAsmGetDialect :: Ptr InlineAsm -> IO AsmDialect
inlineAsmGetDialect = fmap toAsmDialect . inlineAsmGetDialect_
#endif
