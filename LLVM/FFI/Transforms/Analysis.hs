module LLVM.FFI.Transforms.Analysis
       (createGlobalsModRefPass
       ,createAliasDebugger
       ,createAliasAnalysisCounterPass
       ,createAAEvalPass
       ,createNoAAPass
       ,createBasicAliasAnalysisPass
       --,createLibCallAliasAnalysisPass
       ,createScalarEvolutionAliasAnalysisPass
       ,createTypeBasedAliasAnalysisPass
#if HS_LLVM_VERSION>=300
       ,createObjCARCAliasAnalysisPass
#endif
       ,createLazyValueInfoPass
#if HS_LLVM_VERSION>=302
       ,createDependenceAnalysisPass
       ,createCostModelAnalysisPass
#endif
#if HS_LLVM_VERSION>=304
       ,createDelinearizationPass
#endif
       ,createInstCountPass
       ,createRegionInfoPass
       ,createModuleDebugInfoPrinterPass
#if HS_LLVM_VERSION>=209
       ,createMemDepPrinter
#endif
#if HS_LLVM_VERSION>=307
#elif HS_LLVM_VERSION>=305
       ,createJumpInstrTableInfoPass
#endif
       ) where

import LLVM.FFI.Interface
