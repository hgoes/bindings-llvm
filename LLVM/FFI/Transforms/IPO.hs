module LLVM.FFI.Transforms.IPO
       (createStripSymbolsPass
       ,createStripNonDebugSymbolsPass
       ,createStripDebugDeclarePass
       ,createStripDeadDebugInfoPass
       ,createConstantMergePass
       ,createGlobalOptimizerPass
       ,createGlobalDCEPass
       ,createFunctionInliningPass
       ,createAlwaysInlinerPass
       ,createPruneEHPass
       ,createInternalizePass
       ,createDeadArgEliminationPass
       ,createDeadArgHackingPass
       ,createArgumentPromotionPass
       ,createIPConstantPropagationPass
       ,createIPSCCPPass
       ,createLoopExtractorPass
       ,createSingleLoopExtractorPass
       ,createBlockExtractorPass
       ,createStripDeadPrototypesPass
#if HS_LLVM_VERSION >= 308
       ,createPostOrderFunctionAttrsPass
       ,createReversePostOrderFunctionAttrsPass
#else
       ,createFunctionAttrsPass
#endif
       ,createMergeFunctionsPass
       ,createPartialInliningPass
#if HS_LLVM_VERSION >= 303
       ,createMetaRenamerPass
       ,createBarrierNoopPass
#endif
) where

import LLVM.FFI.Interface
