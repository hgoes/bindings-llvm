module LLVM.FFI.DebugLoc 
       (DebugLoc(),
        newDebugLoc,
        debugLocGetLine,
        debugLocGetCol,
        debugLocGetScope,
#if HS_LLVM_VERSION >= 300
        debugLocGetInlinedAt,
        debugLocDump
#else
        debugLocGetInlinedAt
#endif
       ) where

import LLVM.FFI.Interface