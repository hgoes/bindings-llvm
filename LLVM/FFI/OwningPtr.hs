module LLVM.FFI.OwningPtr 
       (OwningPtr(),
        OwningPtrC(..)) where

import Foreign
import LLVM.FFI.Interface

class OwningPtrC a where
  newOwningPtr :: Ptr a -> IO (Ptr (OwningPtr a))
  deleteOwningPtr :: Ptr (OwningPtr a) -> IO ()
  takeOwningPtr :: Ptr (OwningPtr a) -> IO (Ptr a)