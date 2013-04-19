module LLVM.FFI.BasicBlock
       (BasicBlock()
       ,basicBlockGetParent
       ,getInstList
       ,getTerminator
       )where

import LLVM.FFI.Interface
import LLVM.FFI.OOP
import LLVM.FFI.Value
import LLVM.FFI.Constant
import LLVM.FFI.Instruction
import LLVM.FFI.IPList
import LLVM.FFI.CPP

import Foreign
import Foreign.C

#include "Helper.h"

TYPE_LEAF(BasicBlock)
SUBTYPE(Value,BasicBlock)

SPECIALIZE_IPLIST(BasicBlock,capi)
SPECIALIZE_VECTOR(BasicBlock)