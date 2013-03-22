module LLVM.FFI.DebugLoc 
       (DebugLoc(),
        newDebugLoc,
        debugLocGetLine,
        debugLocGetCol,
        debugLocGetScope,
        debugLocGetInlinedAt,
        debugLocDump
       ) where

import LLVM.FFI.Interface