module LLVM.FFI.Function 
       (Function()
       ,createFunction
       ,deleteFunction
       ,functionIsVarArg
       ,getBasicBlockList
       ,getEntryBlock
       ,functionGetFunctionType
       ,functionGetArgumentList
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.IPList
import LLVM.FFI.BasicBlock

import Foreign
import Foreign.C

#include "Helper.h"

SPECIALIZE_IPLIST(Function,capi)

createFunction :: Ptr FunctionType -> LinkageTypes -> Ptr Twine -> Ptr Module -> IO (Ptr Function)
createFunction tp lnk name mod
  = createFunction_ tp (fromLinkageTypes lnk) name mod
