module LLVM.FFI.Transforms.Scalar 
       (createConstantPropagationPass
       ,createSCCPPass
       ,createDeadInstEliminationPass
       ,createDeadCodeEliminationPass
       ,createDeadStoreEliminationPass
       ,createAggressiveDCEPass
#if HS_LLVM_VERSION>=302
       ,createSROAPass
#endif
       ,createScalarReplAggregatesPass
       ,createIndVarSimplifyPass
       ,createInstructionCombiningPass
       ,createLICMPass
       ,createLoopStrengthReducePass
#if HS_LLVM_VERSION>=301
       ,createGlobalMergePass
#endif
       ,createLoopUnswitchPass
#if HS_LLVM_VERSION>=209
       ,createLoopInstSimplifyPass
#endif
       ,createLoopUnrollPass
#if HS_LLVM_VERSION>=305
       ,createSimpleLoopUnrollPass
#endif
#if HS_LLVM_VERSION>=304
       ,createLoopRerollPass
#endif
       ,createLoopRotatePass
#if HS_LLVM_VERSION>=209
       ,createLoopIdiomPass
#endif
       ,createPromoteMemoryToRegisterPass
       ,createDemoteRegisterToMemoryPass
       ,createReassociatePass
       ,createJumpThreadingPass
       ,createCFGSimplificationPass
#if HS_LLVM_VERSION>=304
       ,createFlattenCFGPass
       ,createStructurizeCFGPass
#endif
       ,createBreakCriticalEdgesPass
       ,createLoopSimplifyPass
       ,createTailCallEliminationPass
       ,createLowerSwitchPass
       ,createLowerInvokePass
       ,createLCSSAPass
#if HS_LLVM_VERSION>=209
       ,createEarlyCSEPass
#endif
{- #if HS_LLVM_VERSION>=305
       ,createMergedLoadStoreMotionPass
 #endif -}
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
#if HS_LLVM_VERSION>=209
       ,createInstructionSimplifierPass
#endif
#if HS_LLVM_VERSION>=300
       ,createLowerExpectIntrinsicPass
#endif
#if HS_LLVM_VERSION>=304
       ,createPartiallyInlineLibCallsPass
       ,createSampleProfileLoaderPass
#endif
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
