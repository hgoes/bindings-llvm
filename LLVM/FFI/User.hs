{-# OPTIONS -cpp -pgmPcpphs -optP--cpp #-}
module LLVM.FFI.User 
       (User(),UserC(),
        Operator(),
        getNumOperands,
        getOperand,
        setOperand,
        getOperandUse,
        replaceUsesOfWith)
       where

import LLVM.FFI.OOP
import LLVM.FFI.Value
import LLVM.FFI.Interface

import Foreign
import Foreign.C

#include "Helper.h"

TYPE(User)
SUBTYPE(Value,User)
TYPE_LEAF(Operator)
SUBTYPE2(Value,User,Operator)

GETTYPE(User)
GETTYPE(Operator)

getNumOperands :: UserC t => Ptr t -> IO CUInt
getNumOperands = getNumOperands_

getOperand :: UserC t => Ptr t -> CUInt -> IO (Ptr Value)
getOperand = getOperand_

setOperand :: (UserC t,ValueC val) => Ptr t -> CUInt -> Ptr val -> IO ()
setOperand = setOperand_

getOperandUse :: UserC t => Ptr t -> CUInt -> IO (Ptr Use)
getOperandUse = getOperandUse_

replaceUsesOfWith :: (UserC t,ValueC v1,ValueC v2)
                     => Ptr t -> Ptr v1 -> Ptr v2 -> IO ()
replaceUsesOfWith = replaceUsesOfWith_
