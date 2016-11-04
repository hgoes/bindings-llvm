module LLVM.FFI.Type
       ( -- * Types
         Type(..)
        ,TypeC()
        ,peekType
        ,pokeType
        ,typeGetContext
        ,isVoidType
        ,getVoidType
#if HS_LLVM_VERSION>=301
        ,isHalfType
        ,getHalfType
#endif
        ,isFloatType
        ,getFloatType
        ,isDoubleType
        ,getDoubleType
        ,isX86_FP80Type
        ,getX86_FP80Type
        ,isFP128Type
        ,getFP128Type
        ,isPPC_FP128Type
        ,getPPC_FP128Type
        ,isFloatingPointType
#if HS_LLVM_VERSION>=209
        ,isX86_MMXType
        ,getX86_MMXType
#endif
        ,isLabelType
        ,getLabelType
        ,isMetadataType
        ,getMetadataType
        ,typeDump
         -- ** Integer Type
        ,IntegerType(..)
        ,peekIntegerType
        ,pokeIntegerType
        ,getBitWidth
        ,getIntegerType
         -- ** Function Type
        ,FunctionType(..)
        ,peekFunctionType
        ,pokeFunctionType
        ,newFunctionType
        ,functionTypeIsVarArg
        ,functionTypeGetReturnType
        ,functionTypeGetNumParams
        ,functionTypeGetParamType
         -- ** Composite Types
        ,CompositeType(..)
        ,CompositeTypeC()
        ,peekCompositeType
        ,pokeCompositeType
        ,compositeTypeGetTypeAtIndex
        ,compositeTypeIndexValid
         -- *** Struct Type
        ,StructType(..)
        ,peekStructType
        ,pokeStructType
        ,newStructType
#if HS_LLVM_VERSION>=300
        ,newNamedStructType
#endif
        ,structTypeIsPacked
#if HS_LLVM_VERSION>=300
        ,structTypeHasName
        ,structTypeGetName
#endif
        ,structTypeGetNumElements
        ,structTypeGetElementType
         -- *** Sequential Types
        ,SequentialType(..)
        ,SequentialTypeC()
        ,peekSequentialType
        ,pokeSequentialType
        ,sequentialTypeGetElementType
         -- **** Array Type
        ,ArrayType(..)
        ,peekArrayType
        ,pokeArrayType
        ,arrayTypeGetNumElements
         -- **** Pointer Type
        ,PointerType(..)
        ,peekPointerType
        ,pokePointerType
        ,pointerTypeGet
        ,pointerTypeGetAddressSpace
         -- **** Vector Type
        ,VectorType(..)
        ,peekVectorType
        ,pokeVectorType
        ,vectorTypeGet
        ,vectorTypeGetNumElements
        ) where

import LLVM.FFI.OOP
import LLVM.FFI.Context
import LLVM.FFI.ArrayRef
import LLVM.FFI.SetVector
import LLVM.FFI.CPP
import LLVM.FFI.Interface

import Foreign
import Foreign.C
import Foreign.C.String

import System.IO.Unsafe (unsafeInterleaveIO)

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

#if HS_LLVM_VERSION >= 209
SPECIALIZE_ARRAYREF(Type)
#endif
SPECIALIZE_VECTOR(Type)
SPECIALIZE_SETVECTOR(Type)

typeDump :: TypeC t => Ptr t -> IO ()
typeDump = typeDump_

typeGetContext :: TypeC t => Ptr t -> IO (Ptr LLVMContext)
typeGetContext = typeGetContext_

getBitWidth :: Ptr IntegerType -> IO Integer
getBitWidth ptr = fmap fromIntegral (getBitWidth_ ptr)

getIntegerType :: Ptr LLVMContext -> Integer -> IO (Ptr IntegerType)
getIntegerType ctx bw = getIntegerType_ ctx (fromIntegral bw)

functionTypeGetNumParams :: Ptr FunctionType -> IO Integer
functionTypeGetNumParams ptr = fmap fromIntegral (functionTypeGetNumParams_ ptr)

functionTypeGetParamType :: Ptr FunctionType -> Integer -> IO (Ptr Type)
functionTypeGetParamType ptr i = functionTypeGetParamType_ ptr (fromIntegral i)

#if HS_LLVM_VERSION>=300
newFunctionType :: TypeC rtp => Ptr rtp -> Ptr (ArrayRef (Ptr Type)) -> Bool -> IO (Ptr FunctionType)
#else
newFunctionType :: TypeC rtp => Ptr rtp -> Ptr (Vector (Ptr Type)) -> Bool -> IO (Ptr FunctionType)
#endif
newFunctionType = newFunctionType_

isVoidType :: TypeC t => Ptr t -> Bool
isVoidType = isVoidTy_

#if HS_LLVM_VERSION>=301
isHalfType :: TypeC t => Ptr t -> Bool
isHalfType = isHalfTy_
#endif

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

#if HS_LLVM_VERSION>=209
isX86_MMXType :: TypeC t => Ptr t -> Bool
isX86_MMXType = isX86_MMXTy_
#endif

isLabelType :: TypeC t => Ptr t -> Bool
isLabelType = isLabelTy_

isMetadataType :: TypeC t => Ptr t -> Bool
isMetadataType = isMetadataTy_

structTypeGetNumElements :: Ptr StructType -> IO Integer
structTypeGetNumElements ptr = fmap toInteger $ structTypeGetNumElements_ ptr

structTypeGetElementType :: Ptr StructType -> Integer -> IO (Ptr Type)
structTypeGetElementType ptr idx = structTypeGetElementType_ ptr (fromInteger idx)

pointerTypeGetAddressSpace :: Ptr PointerType -> IO Integer
pointerTypeGetAddressSpace ptr = fmap toInteger (pointerTypeGetAddressSpace_ ptr)

pointerTypeGet :: TypeC t => Ptr t -> Integer -> IO (Ptr PointerType)
pointerTypeGet ptr addr = pointerTypeGet_ ptr (fromInteger addr)

sequentialTypeGetElementType :: SequentialTypeC t => Ptr t -> IO (Ptr Type)
sequentialTypeGetElementType = sequentialTypeGetElementType_

arrayTypeGetNumElements :: Ptr ArrayType -> IO Integer
arrayTypeGetNumElements ptr = fmap toInteger $ arrayTypeGetNumElements_ ptr

arrayTypeGet :: TypeC t => Ptr t -> Integer -> IO (Ptr ArrayType)
arrayTypeGet p n = arrayTypeGet_ p (fromInteger n)

compositeTypeGetTypeAtIndex :: CompositeTypeC t => Ptr t -> Integer -> IO (Ptr Type)
compositeTypeGetTypeAtIndex ptr idx = compositeTypeGetTypeAtIndex_ ptr (fromInteger idx)

compositeTypeIndexValid :: CompositeTypeC t => Ptr t -> Integer -> IO Bool
compositeTypeIndexValid ptr idx = compositeTypeIndexValid_ ptr (fromInteger idx)

vectorTypeGetNumElements :: Ptr VectorType -> IO Integer
vectorTypeGetNumElements ptr = fmap toInteger (vectorTypeGetNumElements_ ptr)

vectorTypeGet :: TypeC t => Ptr t -> Integer -> IO (Ptr VectorType)
vectorTypeGet tp num = vectorTypeGet_ tp (fromInteger num)

peekType :: Ptr Type -> IO Type
peekType (isVoidType -> True) = return VoidType
#if HS_LLVM_VERSION>=301
peekType (isHalfType -> True) = return HalfType
#endif
peekType (isFloatType -> True) = return FloatType
peekType (isDoubleType -> True) = return DoubleType
peekType (isX86_FP80Type -> True) = return X86_FP80Type
peekType (isPPC_FP128Type -> True) = return PPC_FP128Type
#if HS_LLVM_VERSION>=209
peekType (isX86_MMXType -> True) = return X86_MMXType
#endif
peekType (isLabelType -> True) = return LabelType
peekType (isMetadataType -> True) = return MetadataType
peekType (castDown -> Just itp) = fmap IntType $ peekIntegerType itp
peekType (castDown -> Just ftp) = fmap FunType $ peekFunctionType ftp
peekType (castDown -> Just ctp) = fmap CompType $ peekCompositeType ctp

pokeType :: Ptr LLVMContext -> Type -> IO (Ptr Type)
pokeType ctx VoidType = getVoidType ctx
#if HS_LLVM_VERSION>=301
pokeType ctx HalfType = getHalfType ctx
#else
pokeType ctx HalfType = error "HalfType not a supported type in LLVM < 3.0"
#endif
pokeType ctx FloatType = getFloatType ctx
pokeType ctx DoubleType = getDoubleType ctx
pokeType ctx X86_FP80Type = getX86_FP80Type ctx
pokeType ctx FP128Type = getFP128Type ctx
pokeType ctx PPC_FP128Type = getPPC_FP128Type ctx
#if HS_LLVM_VERSION>=209
pokeType ctx X86_MMXType = getX86_MMXType ctx
#else
pokeType ctx X86_MMXType = error "X86_MMXType not a supported type in LLVM < 2.9"
#endif
pokeType ctx LabelType = getLabelType ctx
pokeType ctx MetadataType = getMetadataType ctx
pokeType ctx (IntType itp) = fmap castUp $ pokeIntegerType ctx itp
pokeType ctx (FunType ftp) = fmap castUp $ pokeFunctionType ctx ftp
pokeType ctx (CompType ctp) = fmap castUp $ pokeCompositeType ctx ctp

peekIntegerType :: Ptr IntegerType -> IO IntegerType
peekIntegerType ptr = do
  bw <- getBitWidth_ ptr
  return $ IntegerType bw

pokeIntegerType :: Ptr LLVMContext -> IntegerType -> IO (Ptr IntegerType)
pokeIntegerType ctx (IntegerType bw) = getIntegerType_ ctx bw

peekFunctionType :: Ptr FunctionType -> IO FunctionType
peekFunctionType ptr = do
  va <- functionTypeIsVarArg ptr
  num <- functionTypeGetNumParams_ ptr
  args <- mapM (\n -> functionTypeGetParamType_ ptr n >>= peekType
               ) [0..num-1]
  ret <- functionTypeGetReturnType ptr >>= peekType
  return $ FunctionType va args ret

pokeFunctionType :: Ptr LLVMContext -> FunctionType -> IO (Ptr FunctionType)
pokeFunctionType ctx (FunctionType va par ret) = do
  ret' <- pokeType ctx ret
  par' <- mapM (pokeType ctx) par
#if HS_LLVM_VERSION>=300
  withArrayRef par' $
#else
  withVector par' $
#endif
    \par'' -> newFunctionType_ ret' par'' va

peekCompositeType :: Ptr CompositeType -> IO CompositeType
peekCompositeType (castDown -> Just stp)
  = fmap StructType_ $ peekStructType stp
peekCompositeType (castDown -> Just stp)
  = fmap SequentialType $ peekSequentialType stp

pokeCompositeType :: Ptr LLVMContext -> CompositeType -> IO (Ptr CompositeType)
pokeCompositeType ctx (StructType_ stp) = fmap castUp $ pokeStructType ctx stp
pokeCompositeType ctx (SequentialType stp)
  = fmap castUp $ pokeSequentialType ctx stp

peekSequentialType :: Ptr SequentialType -> IO SequentialType
peekSequentialType (castDown -> Just atp)
  = fmap ArrType $ peekArrayType atp
peekSequentialType (castDown -> Just ptp)
  = fmap PtrType $ peekPointerType ptp
peekSequentialType (castDown -> Just vtp)
  = fmap VecType $ peekVectorType vtp

pokeSequentialType :: Ptr LLVMContext -> SequentialType
                   -> IO (Ptr SequentialType)
pokeSequentialType ctx (ArrType atp) = fmap castUp $ pokeArrayType ctx atp
pokeSequentialType ctx (PtrType ptp) = fmap castUp $ pokePointerType ctx ptp
pokeSequentialType ctx (VecType vtp) = fmap castUp $ pokeVectorType ctx vtp

peekArrayType :: Ptr ArrayType -> IO ArrayType
peekArrayType ptr = do
  tp <- sequentialTypeGetElementType ptr >>= peekType
  num <- arrayTypeGetNumElements_ ptr
  return $ ArrayType tp num

pokeArrayType :: Ptr LLVMContext -> ArrayType -> IO (Ptr ArrayType)
pokeArrayType ctx (ArrayType tp num) = do
  tp' <- pokeType ctx tp
  arrayTypeGet_ tp' num

peekPointerType :: Ptr PointerType -> IO PointerType
peekPointerType ptr = do
  tp <- sequentialTypeGetElementType ptr >>= peekType
  addrSp <- pointerTypeGetAddressSpace_ ptr
  return $ PointerType tp addrSp

pokePointerType :: Ptr LLVMContext -> PointerType -> IO (Ptr PointerType)
pokePointerType ctx (PointerType tp addrSp) = do
  tp' <- pokeType ctx tp
  pointerTypeGet_ tp' addrSp

peekVectorType :: Ptr VectorType -> IO VectorType
peekVectorType ptr = do
  tp <- sequentialTypeGetElementType ptr >>= peekType
  num <- vectorTypeGetNumElements_ ptr
  return $ VectorType tp num

pokeVectorType :: Ptr LLVMContext -> VectorType -> IO (Ptr VectorType)
pokeVectorType ctx (VectorType tp num) = do
  tp' <- pokeType ctx tp
  vectorTypeGet_ tp' num

peekStructType :: Ptr StructType -> IO StructType
peekStructType ptr = do
  packed <- structTypeIsPacked ptr
#if HS_LLVM_VERSION>=300
  hasN <- structTypeHasName ptr
  name <- if hasN
          then fmap Just $ structTypeGetName ptr
               >>= stringRefData_
               >>= peekCString
          else return Nothing
#else
  let name = Nothing
#endif
  numTps <- structTypeGetNumElements_ ptr
  tps <- unsafeInterleaveIO $
         mapM (\n -> structTypeGetElementType_ ptr n >>= peekType
              ) [0..numTps-1]
  return $ StructType packed name tps

pokeStructType :: Ptr LLVMContext -> StructType -> IO (Ptr StructType)
pokeStructType ctx (StructType packed Nothing tps) = do
  tps' <- mapM (pokeType ctx) tps
#if HS_LLVM_VERSION>=300
  withArrayRef tps' $
#else
  withVector tps' $
#endif
    \tps'' -> newStructType ctx tps'' packed
pokeStructType ctx (StructType packed (Just name) tps)
  = error "poking named struct types not supported."
