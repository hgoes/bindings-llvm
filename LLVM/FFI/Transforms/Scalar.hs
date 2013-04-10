module LLVM.FFI.Transforms.Scalar 
       (createCFGSimplificationPass
       ,createConstantPropagationPass
       ,createDemoteRegisterToMemoryPass
       ,createGVNPass
       ,createInstructionCombiningPass
       ,createPromoteMemoryToRegisterPass
       ,createReassociatePass
       ,createAggressiveDCEPass
       ,createDeadStoreEliminationPass
       ,createIndVarSimplifyPass
       ,createJumpThreadingPass
       ,createLICMPass
       ,createLoopDeletionPass
       ,createLoopRotatePass
       ,createLoopSimplifyPass
       ,createLoopStrengthReducePass
       ,createLoopUnrollPass
       ,createLoopUnswitchPass
       ,createMemCpyOptPass
       ,createSCCPPass
       ,createScalarReplAggregatesPass
       ,createSimplifyLibCallsPass
       ,createTailCallEliminationPass
       ,createVerifierPass
       ) where

import LLVM.FFI.Interface