#ifndef LLVM_HASKELL_PASS_H
#define LLVM_HASKELL_PASS_H

#include <HsFFI.h>
#ifdef __cplusplus
#include <llvm/Pass.h>
#else
#include <stdbool.h>
#endif

typedef void (*GetUsageFunctionT)(const void*,void*);
typedef bool (*RunOnModuleFunctionT)(void*,void*);
typedef bool (*InitializationFunctionT)(void*,void*);
typedef bool (*FinalizationFunctionT)(void*,void*);
typedef bool (*RunOnFunctionFunctionT)(void*,void*);


#ifdef __cplusplus

class HaskellModulePass : public llvm::ModulePass {
  GetUsageFunctionT GetUsage;
  RunOnModuleFunctionT RunOnModule;
public:
  HaskellModulePass();
  HaskellModulePass(GetUsageFunctionT usage,RunOnModuleFunctionT run);
  ~HaskellModulePass();
  void getAnalysisUsage(llvm::AnalysisUsage &Info) const;
  bool runOnModule(llvm::Module& m);
  static char ID;
};

class HaskellFunctionPass : public llvm::FunctionPass {
  GetUsageFunctionT GetUsage;
  InitializationFunctionT Initialization;
  FinalizationFunctionT Finalization;
  RunOnFunctionFunctionT RunOnFunction;
public:
  HaskellFunctionPass();
  HaskellFunctionPass(GetUsageFunctionT usage,
                        InitializationFunctionT init,
                        FinalizationFunctionT fin,
                        RunOnFunctionFunctionT run);
  ~HaskellFunctionPass();
  void getAnalysisUsage(llvm::AnalysisUsage &Info) const;
  bool doInitialization(llvm::Module& m);
  bool doFinalization(llvm::Module& m);
  bool runOnFunction(llvm::Function& f);
  static char ID;
};

#endif

#ifdef __cplusplus
extern "C" {
#endif

void* newHaskellModulePass(GetUsageFunctionT,RunOnModuleFunctionT);
void deleteHaskellModulePass(void*);

void* newHaskellFunctionPass(GetUsageFunctionT,InitializationFunctionT,FinalizationFunctionT,RunOnFunctionFunctionT);
void deleteHaskellFunctionPass(void*);

#ifdef __cplusplus
}
#endif

#endif
