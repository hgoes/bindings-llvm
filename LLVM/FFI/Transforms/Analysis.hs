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
       ,createObjCARCAliasAnalysisPass
       ,createLazyValueInfoPass
       ,createDependenceAnalysisPass
       ,createCostModelAnalysisPass
       ,createDelinearizationPass
       ,createInstCountPass
       ,createRegionInfoPass
       ,createModuleDebugInfoPrinterPass
       ,createMemDepPrinter
#if HS_LLVM_VERSION>=305
       ,createJumpInstrTableInfoPass
#endif
       ) where

import LLVM.FFI.Interface
