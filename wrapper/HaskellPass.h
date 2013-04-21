#ifndef LLVM_HASKELL_PASS_H
#define LLVM_HASKELL_PASS_H

#include <llvm/Pass.h>
#include <HsFFI.h>

class HaskellPassBase {
  HsFunPtr GetUsageFunction;
public:
  HaskellPassBase(HsFunPtr usage);
  ~HaskellPassBase();
  void getAnalysisUsage(const void* self,llvm::AnalysisUsage &Info) const;
};

class HaskellPassInitBase {
  HsFunPtr InitializationFunction;
  HsFunPtr FinalizationFunction;
public:
  HaskellPassInitBase(HsFunPtr init,HsFunPtr fin);
  ~HaskellPassInitBase();
  bool doInitialization(void* self,llvm::Module& m);
  bool doFinalization(void* self,llvm::Module& m);
};

class HaskellModulePassBase {
  HsFunPtr RunOnModuleFunction;
public:
  HaskellModulePassBase(HsFunPtr run);
  ~HaskellModulePassBase();
  bool runOnModule(void* self,llvm::Module& m);
};

class HaskellFunctionPassBase {
  HsFunPtr RunOnFunctionFunction;
public:
  HaskellFunctionPassBase(HsFunPtr run);
  ~HaskellFunctionPassBase();
  bool runOnFunction(void* self,llvm::Function& f);
};

class HaskellModulePass : public llvm::ModulePass {
  HaskellPassBase base;
  HaskellModulePassBase base_module;
public:
  HaskellModulePass();
  HaskellModulePass(HsFunPtr usage,HsFunPtr run);
  void getAnalysisUsage(llvm::AnalysisUsage &Info) const;
  bool runOnModule(llvm::Module& m);
  static char ID;
};

class HaskellFunctionPass : public llvm::FunctionPass {
  HaskellPassBase base;
  HaskellPassInitBase base_init;
  HaskellFunctionPassBase base_function;
public:
  HaskellFunctionPass();
  HaskellFunctionPass(HsFunPtr usage,HsFunPtr init,HsFunPtr fin,HsFunPtr run);
  void getAnalysisUsage(llvm::AnalysisUsage &Info) const;
  bool doInitialization(llvm::Module& m);
  bool doFinalization(llvm::Module& m);
  bool runOnFunction(llvm::Function& f);
  static char ID;  
};

#endif
