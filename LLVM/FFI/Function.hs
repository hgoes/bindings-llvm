module LLVM.FFI.Function 
       (Function()
       ,functionIsVarArg
       ,getBasicBlockList
       ,getEntryBlock
       ,functionGetFunctionType
       )where

import LLVM.FFI.Interface
import LLVM.FFI.IPList
import LLVM.FFI.BasicBlock

import Foreign
import Foreign.C

#include "Helper.h"

SPECIALIZE_IPLIST(Function,capi)

