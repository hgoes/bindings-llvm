#include "HaskellPass.h"
#include <iostream>

//static llvm::RegisterPass<HaskellModulePass> X1("haskell-module","Haskell module pass",false,false);

char HaskellModulePass::ID = 0;

HaskellModulePass::HaskellModulePass() 
  : llvm::ModulePass(ID),
    GetUsage(0),
    RunOnModule(0) {}

HaskellModulePass::HaskellModulePass(GetUsageFunctionT usage,RunOnModuleFunctionT run)
  : llvm::ModulePass(ID),
    GetUsage(usage),
    RunOnModule(run) {}

HaskellModulePass::~HaskellModulePass() {
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(GetUsage));
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(RunOnModule));
}

void HaskellModulePass::getAnalysisUsage(llvm::AnalysisUsage &Info) const {
  GetUsage(this,(void*)&Info);
}

bool HaskellModulePass::runOnModule(llvm::Module& m) {
  return RunOnModule(this,(void*)&m);
}

const char* HaskellModulePass::getPassName() const {
  return "Haskell Pass";
}

extern "C" {
  void* newHaskellModulePass(GetUsageFunctionT usage,RunOnModuleFunctionT run) {
    return (void*)new HaskellModulePass(usage,run);
  }
  void deleteHaskellModulePass(void* pass) {
    delete (HaskellModulePass*)pass;
  }
  const char* passId_HaskellModulePass = &HaskellModulePass::ID;
}

//static llvm::RegisterPass<HaskellFunctionPass> X2("haskell-function","Haskell function pass",false,false);

char HaskellFunctionPass::ID = 0;

HaskellFunctionPass::HaskellFunctionPass() 
  : llvm::FunctionPass(ID),
    GetUsage(0),
    Initialization(0),
    Finalization(0),
    RunOnFunction(0) {}

HaskellFunctionPass::HaskellFunctionPass(GetUsageFunctionT usage,
                                         InitializationFunctionT init,
                                         FinalizationFunctionT fin,
                                         RunOnFunctionFunctionT run)
  : llvm::FunctionPass(ID),
    GetUsage(usage),
    Initialization(init),
    Finalization(fin),
    RunOnFunction(run) {}

HaskellFunctionPass::~HaskellFunctionPass() {
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(GetUsage));
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(Initialization));
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(Finalization));
  hs_free_fun_ptr(reinterpret_cast<HsFunPtr>(RunOnFunction));
}

void HaskellFunctionPass::getAnalysisUsage(llvm::AnalysisUsage &Info) const {
  GetUsage(this,&Info);
}

bool HaskellFunctionPass::doInitialization(llvm::Module& m) {
  return Initialization(this,&m);
}

bool HaskellFunctionPass::doFinalization(llvm::Module& m) {
  return Finalization(this,&m);
}

bool HaskellFunctionPass::runOnFunction(llvm::Function& f) {
  return RunOnFunction(this,&f);
}

extern "C" {
  void* newHaskellFunctionPass(GetUsageFunctionT usage,
                               InitializationFunctionT init,
                               FinalizationFunctionT fin,
                               RunOnFunctionFunctionT run) {
    return (void*)new HaskellFunctionPass(usage,init,fin,run);
  }
  void deleteHaskellFunctionPass(void* pass) {
    delete (HaskellFunctionPass*)pass;
  }
}
