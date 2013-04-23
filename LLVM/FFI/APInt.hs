module LLVM.FFI.APInt
       (APInt()
       ,newAPInt
       ,newAPIntLimited
       ,newAPIntFromString
       ,deleteAPInt
       ,apIntGetBitWidth
       ,apIntGetZExtValue
       ,apIntGetSExtValue
       ) where

import LLVM.FFI.Interface
import Foreign
