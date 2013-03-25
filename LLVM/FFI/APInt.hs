module LLVM.FFI.APInt
       (APInt()
       ,apIntGetBitWidth
       ,apIntGetZExtValue
       ,apIntGetSExtValue
       ) where

import LLVM.FFI.Interface
import Foreign

apIntGetBitWidth :: Ptr APInt -> IO Integer
apIntGetBitWidth ptr = fmap toInteger (apIntGetBitWidth_ ptr)