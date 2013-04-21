#ifndef LLVM_HASKELL_PASS_C_H
#define LLVM_HASKELL_PASS_C_H

#include <stdbool.h>
#include <HsFFI.h>

extern void* newHaskellModulePass(HsFunPtr,HsFunPtr,HsFunPtr,HsFunPtr);
extern void deleteHaskellModulePass(void*);

extern void* newHaskellFunctionPass(HsFunPtr,HsFunPtr,HsFunPtr,HsFunPtr);
extern void deleteHaskellFunctionPass(void*);

#endif
