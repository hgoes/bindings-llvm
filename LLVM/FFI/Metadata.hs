module LLVM.FFI.Metadata
       (MDNode(),
        newMDNode,
        mdNodeGetNumOperands,
        mdNodeGetOperand,
        mdNodeIsFunctionLocal,
        mdNodeGetFunction,
        MDString(),
        newMDString,
        mdStringGetString,
        NamedMDNode(),
        namedMDNodeGetParent,
        namedMDNodeGetOperand,
        namedMDNodeGetNumOperands,
        namedMDNodeAddOperand,
        namedMDNodeGetName
       ) where

import LLVM.FFI.Interface
import LLVM.FFI.OOP
import LLVM.FFI.Value

#include "Helper.h"

TYPE_LEAF(MDNode)
SUBTYPE(Value,MDNode)
TYPE_LEAF(MDString)
SUBTYPE(Value,MDString)

GETTYPE(MDNode)
GETTYPE(MDString)
