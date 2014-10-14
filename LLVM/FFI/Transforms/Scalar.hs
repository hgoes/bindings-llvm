module LLVM.FFI.Transforms.Scalar 
       (createConstantPropagationPass
       ,createSCCPPass
       ,createDeadInstEliminationPass
       ,createDeadCodeEliminationPass
       ,createDeadStoreEliminationPass
       ,createAggressiveDCEPass
       ,createSROAPass
       ,createScalarReplAggregatesPass
       ,createIndVarSimplifyPass
       ,createInstructionCombiningPass
       ,createLICMPass
       ,createLoopStrengthReducePass
       ,createGlobalMergePass
       ,createLoopUnswitchPass
       ,createLoopInstSimplifyPass
       ,createLoopUnrollPass
#if HS_LLVM_VERSION>=305
       ,createSimpleLoopUnrollPass
#endif
       ,createLoopRerollPass
       ,createLoopRotatePass
       ,createLoopIdiomPass
       ,createPromoteMemoryToRegisterPass
       ,createDemoteRegisterToMemoryPass
       ,createReassociatePass
       ,createJumpThreadingPass
       ,createCFGSimplificationPass
       ,createFlattenCFGPass
       ,createStructurizeCFGPass
       ,createBreakCriticalEdgesPass
       ,createLoopSimplifyPass
       ,createTailCallEliminationPass
       ,createLowerSwitchPass
       ,createLowerInvokePass
       ,createLCSSAPass
       ,createEarlyCSEPass
#if HS_LLVM_VERSION>=305
       ,createMergedLoadStoreMotionPass
#endif
       ,createGVNPass
       ,createMemCpyOptPass
       ,createLoopDeletionPass
#if HS_LLVM_VERSION>=305
       ,createConstantHoistingPass
#endif
       ,createInstructionNamerPass
       ,createSinkingPass
       ,createLowerAtomicPass
       ,createCorrelatedValuePropagationPass
       ,createInstructionSimplifierPass
       ,createLowerExpectIntrinsicPass
       ,createPartiallyInlineLibCallsPass
       ,createSampleProfileLoaderPass
#if HS_LLVM_VERSION>=305
       ,createScalarizerPass
       ,createAddDiscriminatorsPass
       ,createSeparateConstOffsetFromGEPPass
       ,createLoadCombinePass
#endif
#if HS_LLVM_VERSION<=302
       ,createSimplifyLibCallsPass
#endif
       ) where

import LLVM.FFI.Interface
