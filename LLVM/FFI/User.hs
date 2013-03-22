{-# OPTIONS -cpp -pgmPcpphs -optP--cpp #-}
module LLVM.FFI.User 
       (User(),UserC(),
        Operator(),
        getNumOperands,
        getOperand)
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

getNumOperands :: UserC t => Ptr t -> IO Integer
getNumOperands ptr = fmap fromIntegral (getNumOperands_ ptr)

getOperand :: UserC t => Ptr t -> Integer -> IO (Ptr Value)
getOperand ptr idx = getOperand_ ptr (fromIntegral idx)