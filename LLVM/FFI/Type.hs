module LLVM.FFI.Type
       (Type() 
       ,TypeC()
       ,IntegerType()
       ,FunctionType()
       ,CompositeType()
       ,CompositeTypeC()
       ,StructType()
       ,SequentialType()
       ,SequentialTypeC()
       ,ArrayType()
       ,PointerType()
       ,VectorType()
       ,isVoidType
       ,isHalfType
       ,isFloatType
       ,isDoubleType
       ,isX86_FP80Type
       ,isFP128Type
       ,isPPC_FP128Type
       ,isFloatingPointType
       ,isX86_MMXType
       ,isLabelType
       ,typeDump 
       ,getBitWidth
       ,getIntegerType
       ,isVarArg
       ,getReturnType
       ,structTypeIsPacked
       ,structTypeHasName
       ,structTypeGetName
       ,structTypeGetNumElements
       ,structTypeGetElementType
       ,pointerTypeGet
       ,pointerTypeGetAddressSpace
       ) where

import LLVM.FFI.OOP
import LLVM.FFI.Context
import LLVM.FFI.ArrayRef
import LLVM.FFI.Interface

import Foreign
import Foreign.C

#include "Helper.h"

TYPE(Type)
TYPE_LEAF(IntegerType)
SUBTYPE(Type,IntegerType)
TYPE_LEAF(FunctionType)
SUBTYPE(Type,FunctionType)
TYPE(CompositeType)
SUBTYPE(Type,CompositeType)
TYPE_LEAF(StructType)
SUBTYPE2(Type,CompositeType,StructType)
TYPE(SequentialType)
SUBTYPE2(Type,CompositeType,SequentialType)
TYPE_LEAF(ArrayType)
SUBTYPE3(Type,CompositeType,SequentialType,ArrayType)
TYPE_LEAF(PointerType)
SUBTYPE3(Type,CompositeType,SequentialType,PointerType)
TYPE_LEAF(VectorType)
SUBTYPE3(Type,CompositeType,SequentialType,VectorType)

SPECIALIZE_ARRAYREF(Type,capi)

typeDump :: TypeC t => Ptr t -> IO ()
typeDump = typeDump_

getBitWidth :: Ptr IntegerType -> IO Integer
getBitWidth ptr = fmap fromIntegral (getBitWidth_ ptr)

getIntegerType :: Ptr LLVMContext -> Integer -> IO (Ptr IntegerType)
getIntegerType ctx bw = getIntegerType_ ctx (fromIntegral bw)

getNumParams :: Ptr FunctionType -> IO Integer
getNumParams ptr = fmap fromIntegral (getNumParams_ ptr)

getParamType :: Ptr FunctionType -> Integer -> IO (Ptr Type)
getParamType ptr i = getParamType_ ptr (fromIntegral i)

isVoidType :: TypeC t => Ptr t -> Bool
isVoidType = isVoidTy_

isHalfType :: TypeC t => Ptr t -> Bool
isHalfType = isHalfTy_

isFloatType :: TypeC t => Ptr t -> Bool
isFloatType = isFloatTy_

isDoubleType :: TypeC t => Ptr t -> Bool
isDoubleType = isDoubleTy_

isX86_FP80Type :: TypeC t => Ptr t -> Bool
isX86_FP80Type = isX86_FP80Ty_

isFP128Type :: TypeC t => Ptr t -> Bool
isFP128Type = isFP128Ty_

isPPC_FP128Type :: TypeC t => Ptr t -> Bool
isPPC_FP128Type = isPPC_FP128Ty_

isFloatingPointType :: TypeC t => Ptr t -> Bool
isFloatingPointType = isFloatingPointTy_

isX86_MMXType :: TypeC t => Ptr t -> Bool
isX86_MMXType = isX86_MMXTy_

isLabelType :: TypeC t => Ptr t -> Bool
isLabelType = isLabelTy_

structTypeGetNumElements :: Ptr StructType -> IO Integer
structTypeGetNumElements ptr = fmap toInteger $ structTypeGetNumElements_ ptr

structTypeGetElementType :: Ptr StructType -> Integer -> IO (Ptr Type)
structTypeGetElementType ptr idx = structTypeGetElementType_ ptr (fromInteger idx)

pointerTypeGetAddressSpace :: Ptr PointerType -> IO Integer
pointerTypeGetAddressSpace ptr = fmap toInteger (pointerTypeGetAddressSpace_ ptr)

pointerTypeGet :: TypeC t => Ptr t -> Integer -> IO (Ptr PointerType)
pointerTypeGet ptr addr = pointerTypeGet_ ptr (fromInteger addr)