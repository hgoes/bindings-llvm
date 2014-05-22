module LLVM.FFI.APInt
       (APInt(..)
       ,newAPInt
       ,newAPIntLimited
       ,newAPIntFromString
       ,mallocAPInt
       ,deleteAPInt
       ,apIntGetBitWidth
       ,apIntGetZExtValue
       ,apIntGetSExtValue
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.ArrayRef

import Foreign.C
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits
import Data.Word

foreign import ccall unsafe "sizeof_APInt"
  sizeof_APInt :: CSize

foreign import ccall unsafe "alignof_APInt"
  alignof_APInt :: CSize

instance Storable APInt where
  sizeOf _ = fromIntegral sizeof_APInt
  alignment _ = fromIntegral alignof_APInt
  peek ptr = do
    bw <- apIntGetBitWidth ptr
    words <- apIntGetNumWords ptr
    dat <- apIntGetRawData ptr
    val <- peekAPInt 0 (fromIntegral words) dat 0
    return $ APInt (fromIntegral bw) val
    where
      peekAPInt i limit arr v
        | i==limit = return v
        | otherwise = do
          w <- peekElemOff arr i
          peekAPInt (i+1) limit arr (v .|. ((fromIntegral w) `shiftL` (i*64)))
  poke ptr (APInt bw val)
    | bw <= 64 = apIntMoveSimple ptr (fromIntegral bw) (fromIntegral val)
    | otherwise = allocaArray numW $ \arr -> do
      writeData numW arr val
      apIntMove ptr (fromIntegral bw) (fromIntegral numW) arr
    where
      (numW',rest) = bw `divMod` 64
      numW = if rest==0
             then numW'
             else numW'+1
      writeData 0 _ _ = return ()
      writeData n arr val = do
        poke arr (fromIntegral val)
        writeData (n-1) (advancePtr arr 1) (val `shiftR` 64)

foreign import capi "extra.h move_APInt"
  apIntMove :: Ptr APInt -> CUInt -> CUInt -> Ptr Word64 -> IO ()

foreign import capi "extra.h move_APIntSimple"
  apIntMoveSimple :: Ptr APInt -> CUInt -> Word64 -> IO ()

mallocAPInt :: APInt -> IO (Ptr APInt)
mallocAPInt (APInt bw val)
  | bw <= 64 = newAPIntLimited (fromIntegral bw) (fromIntegral val) False
  | otherwise = allocaArray numW $
                \arr -> do
                  writeData numW arr val
#if HS_LLVM_VERSION>=300
                  ref <- newArrayRef' arr (fromIntegral numW)
                  res <- newAPInt (fromIntegral bw) ref
                  deleteArrayRef ref
                  return res
#else
                  newAPInt (fromIntegral bw) (fromIntegral numW) arr
#endif
  where
    (numW',rest) = bw `divMod` 64
    numW = if rest==0
           then numW'
           else numW'+1

    writeData 0 _ _ = return ()
    writeData n arr val = do
      poke arr (fromIntegral val)
      writeData (n-1) (advancePtr arr 1) (val `shiftR` 64)
