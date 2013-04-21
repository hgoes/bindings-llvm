#include "HaskellPass.h"
#include <iostream>

HaskellPassBase::HaskellPassBase(HsFunPtr usage)
  : GetUsageFunction(usage) {}

HaskellPassBase::~HaskellPassBase() {
  hs_free_fun_ptr(GetUsageFunction);
}

void HaskellPassBase::getAnalysisUsage(const void* self,llvm::AnalysisUsage &Info) const {
  reinterpret_cast<void (*)(const void*,void*)>(GetUsageFunction)(self,(void*)&Info);
}

HaskellPassInitBase::HaskellPassInitBase(HsFunPtr init,HsFunPtr fin) 
  : InitializationFunction(init),
    FinalizationFunction(fin) {
}

HaskellPassInitBase::~HaskellPassInitBase() {
  hs_free_fun_ptr(InitializationFunction);
  hs_free_fun_ptr(FinalizationFunction);
}

bool HaskellPassInitBase::doInitialization(void* self,llvm::Module& m) {
  return reinterpret_cast<bool (*)(void*,void*)>(InitializationFunction)(self,(void*)&m);
}

bool HaskellPassInitBase::doFinalization(void* self,llvm::Module& m) {
  return reinterpret_cast<bool (*)(void*,void*)>(FinalizationFunction)(self,(void*)&m);
}

HaskellModulePassBase::HaskellModulePassBase(HsFunPtr run) 
  : RunOnModuleFunction(run) {
}

HaskellModulePassBase::~HaskellModulePassBase() {
  hs_free_fun_ptr(RunOnModuleFunction);
}

bool HaskellModulePassBase::runOnModule(void* self,llvm::Module& m) {
  return reinterpret_cast<bool (*)(void*,void*)>(RunOnModuleFunction)(self,(void*)&m);
}

HaskellFunctionPassBase::HaskellFunctionPassBase(HsFunPtr run)
  : RunOnFunctionFunction(run) {
}

HaskellFunctionPassBase::~HaskellFunctionPassBase() {
  hs_free_fun_ptr(RunOnFunctionFunction);
}

bool HaskellFunctionPassBase::runOnFunction(void* self,llvm::Function& f) {
  return reinterpret_cast<bool (*)(void*,void*)>(RunOnFunctionFunction)(self,(void*)&f);
}

static llvm::RegisterPass<HaskellModulePass> X1("haskell-module","Haskell module pass",false,false);

char HaskellModulePass::ID = 0;

HaskellModulePass::HaskellModulePass() 
  : llvm::ModulePass(ID),
    base(0),
    base_module(0) {}

HaskellModulePass::HaskellModulePass(HsFunPtr usage,HsFunPtr run)
  : llvm::ModulePass(ID),
    base(usage),
    base_module(run) {
}

void HaskellModulePass::getAnalysisUsage(llvm::AnalysisUsage &Info) const {
  base.getAnalysisUsage(this,Info);
}

bool HaskellModulePass::runOnModule(llvm::Module& m) {
  return base_module.runOnModule(this,m);
}

extern "C" {
  void* newHaskellModulePass(HsFunPtr usage,HsFunPtr run) {
    return (void*)new HaskellModulePass(usage,run);
  }
  void deleteHaskellModulePass(void* pass) {
    delete (HaskellModulePass*)pass;
  }
}

static llvm::RegisterPass<HaskellFunctionPass> X2("haskell-function","Haskell function pass",false,false);

char HaskellFunctionPass::ID = 0;

HaskellFunctionPass::HaskellFunctionPass() 
  : llvm::FunctionPass(ID),
    base(0),
    base_init(0,0),
    base_function(0) {}

HaskellFunctionPass::HaskellFunctionPass(HsFunPtr usage,
                                         HsFunPtr init,
                                         HsFunPtr fin,
                                         HsFunPtr run) 
  : llvm::FunctionPass(ID),
    base(usage),
    base_init(init,fin),
    base_function(run) {
}

void HaskellFunctionPass::getAnalysisUsage(llvm::AnalysisUsage &Info) const {
  base.getAnalysisUsage(this,Info);
}

bool HaskellFunctionPass::doInitialization(llvm::Module& m) {
  base_init.doInitialization(this,m);
}

bool HaskellFunctionPass::doFinalization(llvm::Module& m) {
  base_init.doFinalization(this,m);
}

bool HaskellFunctionPass::runOnFunction(llvm::Function& f) {
  base_function.runOnFunction(this,f);
}

extern "C" {
  void* newHaskellFunctionPass(HsFunPtr usage,HsFunPtr init,HsFunPtr fin,HsFunPtr run) {
    return (void*)new HaskellFunctionPass(usage,init,fin,run);
  }
  void deleteHaskellFunctionPass(void* pass) {
    delete (HaskellFunctionPass*)pass;
  }
}
