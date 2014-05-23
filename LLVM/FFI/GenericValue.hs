module LLVM.FFI.GenericValue
       (GenericValue(..)
       ,newGenericValue
       ,genericValueGetDouble
       ,genericValueSetDouble
       ,genericValueGetFloat
       ,genericValueSetFloat
       ,genericValueGetPointer
       ,genericValueSetPointer
       ,genericValueGetInt
       ,genericValueSetInt
#if HS_LLVM_VERSION>=303
       ,genericValueGetAggregate
       ,genericValueSetAggregate
#endif
       ,peekGenericDouble
       ,peekGenericFloat
       ,peekGenericPointer
       ,peekGenericIntPair
       ,peekGenericInt
#if HS_LLVM_VERSION>=303
       ,peekGenericAggregate
#endif
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.CPP
import LLVM.FFI.APInt

import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Data.List (genericLength)

foreign import ccall unsafe "sizeof_GenericValue"
  sizeof_GenericValue :: CSize

foreign import ccall unsafe "alignof_GenericValue"
  alignof_GenericValue :: CSize

foreign import capi "extra.h genericValueSetIntPair"
  genericValueSetIntPair :: Ptr GenericValue -> CUInt -> CUInt -> IO ()

foreign import capi "extra.h genericValueGetIntPair"
  genericValueGetIntPair :: Ptr GenericValue -> Ptr CUInt -> Ptr CUInt -> IO ()

instance Storable GenericValue where
  sizeOf _ = fromIntegral sizeof_GenericValue
  alignment _ = fromIntegral alignof_GenericValue
  poke addr (GDouble d) = genericValueSetDouble addr d
  poke addr (GFloat f) = genericValueSetFloat addr f
  poke addr (GPointer p) = genericValueSetPointer addr p
  poke addr (GIntPair u1 u2) = genericValueSetIntPair addr u1 u2
  poke addr (GInt ap) = do
    off <- genericValueGetInt addr
    poke off ap
#if HS_LLVM_VERSION>=303
  poke addr (GAggregate els) = do
    arr <- genericValueGetAggregate addr
    vectorResize arr (genericLength els)
    mapM_ (\(idx,el) -> do
              pos <- vectorIndex arr idx
              poke pos el
          ) (zip [0..] els)
#endif
  peek _ = error "bindings-llvm: Cannot peek GenericValues since they are untyped. Use type-specific peekGeneric* functions."
  
peekGenericDouble :: Ptr GenericValue -> IO GenericValue
peekGenericDouble addr = do
  d <- genericValueGetDouble addr
  return $ GDouble d

peekGenericFloat :: Ptr GenericValue -> IO GenericValue
peekGenericFloat addr = do
  f <- genericValueGetFloat addr
  return $ GFloat f

peekGenericPointer :: Ptr GenericValue -> IO GenericValue
peekGenericPointer addr = do
  p <- genericValueGetPointer addr
  return $ GPointer p

peekGenericIntPair :: Ptr GenericValue -> IO GenericValue
peekGenericIntPair addr
  = alloca $
    \i1 -> alloca $
           \i2 -> do
             genericValueGetIntPair addr i1 i2
             r1 <- peek i1
             r2 <- peek i2
             return (GIntPair r1 r2)

peekGenericInt :: Ptr GenericValue -> IO GenericValue
peekGenericInt addr = do
  ap <- genericValueGetInt addr
  val <- peek ap
  return $ GInt val

#if HS_LLVM_VERSION>=303
peekGenericAggregate :: (Ptr GenericValue -> IO GenericValue) -> Ptr GenericValue
                     -> IO GenericValue
peekGenericAggregate recPeek addr = do
  agg <- genericValueGetAggregate addr
  sz <- vectorSize agg
  lst <- mapM (\idx -> do
                  el <- vectorIndex agg idx
                  recPeek el
              ) [0..(sz-1)]
  return $ GAggregate lst
#endif

instance VectorC GenericValue where
  vectorBegin = vectorGenericValueBegin
  vectorEnd = vectorGenericValueEnd
  vectorIteratorDeref = error "bindings-llvm: Can't deref GenericValues since they are untyped."
  vectorIteratorNext = vectorIteratorGenericValueNext
  vectorIteratorEq = vectorIteratorGenericValueEq
  newVector = newVectorGenericValue
  vectorClear = vectorGenericValueClear
  vectorPushBack = vectorGenericValuePushBack
  vectorResize = vectorGenericValueResize
  vectorIndex = vectorGenericValueIndex
  vectorSize = vectorGenericValueSize
