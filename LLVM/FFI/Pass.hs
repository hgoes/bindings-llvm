module LLVM.FFI.Pass
       (Pass()
       ,PassC()
       ,FunctionPass()
       ,createCFGSimplificationPass
       ,passLookupPassInfo
       ) where

import LLVM.FFI.Interface

class PassC t

instance PassC Pass
instance PassC FunctionPass

