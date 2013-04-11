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
       ,createFunctionAttrsPass
       ,createMergeFunctionsPass
       ,createPartialInliningPass
       ,createMetaRenamerPass
       ,createBarrierNoopPass) where

import LLVM.FFI.Interface