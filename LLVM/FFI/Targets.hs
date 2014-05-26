module LLVM.FFI.Targets where

import Data.Map (Map)
import qualified Data.Map as Map

#include "Helper.h"
#include <llvm/Config/llvm-config.h>

data TargetInitialization
  = TargetInitialization { initializeTargetInfo :: IO ()
                         , initializeTarget :: IO ()
                         , initializeTargetMC :: IO ()
                         , initializeAsmPrinter :: Maybe (IO ())
                         , initializeAsmParser :: Maybe (IO ())
                         , initializeDisassembler :: Maybe (IO ())
                         }

#define LLVM_TARGET(name)\
  foreign import ccall "LLVMInitialize##name##TargetInfo"\
    initialize##name##TargetInfo :: IO () ;

#include <llvm/Config/Targets.def>

#define LLVM_TARGET(name)\
  foreign import ccall "LLVMInitialize##name##Target"\
    initialize##name##Target :: IO () ;

#include <llvm/Config/Targets.def>

#define LLVM_TARGET(name)\
  foreign import ccall "LLVMInitialize##name##TargetMC"\
    initialize##name##TargetMC :: IO () ;

#include <llvm/Config/Targets.def>

#define LLVM_ASM_PRINTER(name)\
  foreign import ccall "LLVMInitialize##name##AsmPrinter"\
    initialize##name##AsmPrinter :: IO () ;

#include <llvm/Config/AsmPrinters.def>

#define LLVM_ASM_PARSER(name)\
  foreign import ccall "LLVMInitialize##name##AsmParser"\
    initialize##name##AsmParser :: IO () ;

#include <llvm/Config/AsmParsers.def>

#define LLVM_DISASSEMBLER(name)\
  foreign import ccall "LLVMInitialize##name##Disassembler"\
    initialize##name##Disassembler :: IO () ;

#include <llvm/Config/Disassemblers.def>

nativeTarget :: Maybe TargetInitialization
#ifdef LLVM_NATIVE_ARCH
#define _INIT(arch,name) initialize##arch##name
nativeTarget = Just $ TargetInitialization { initializeTargetInfo = _INIT(LLVM_NATIVE_ARCH,TargetInfo)
                                           , initializeTarget = _INIT(LLVM_NATIVE_ARCH,Target)
                                           , initializeTargetMC = _INIT(LLVM_NATIVE_ARCH,TargetMC)
#ifdef LLVM_NATIVE_ASMPRINTER
                                           , initializeAsmPrinter = Just _INIT(LLVM_NATIVE_ARCH,AsmPrinter)
#else
                                           , initializeAsmPrinter = Nothing
#endif
#ifdef LLVM_NATIVE_ASMPARSER
                                           , initializeAsmParser = Just _INIT(LLVM_NATIVE_ARCH,AsmParser)
#else
                                           , initializeAsmParser = Nothing
#endif
#ifdef LLVM_NATIVE_DISASSEMBLER
                                           , initializeDisassembler = Just _INIT(LLVM_NATIVE_ARCH,Disassembler)
#else
                                           , initializeDisassembler = Nothing
#endif
                                           }
#else
nativeTarget = Nothing
#endif

targets :: Map String TargetInitialization
targets = mp3
  where
    mp0 = Map.fromList (tail [undefined
#define LLVM_TARGET(name) {- -},(#name,TargetInitialization { initializeTargetInfo = initialize##name##TargetInfo\
{-                                                        -}, initializeTarget = initialize##name##Target\
{-                                                        -}, initializeTargetMC = initialize##name##TargetMC\
{-                                                        -}, initializeAsmPrinter = Nothing\
{-                                                        -}, initializeAsmParser = Nothing\
{-                                                        -}, initializeDisassembler = Nothing })
#include <llvm/Config/Targets.def>
                             ])
    mp1 = foldl (\mp (name,fun) -> Map.adjust (\ti -> ti { initializeAsmPrinter = Just fun }
                                              ) name mp) mp0
            (tail [undefined
#define LLVM_ASM_PRINTER(name) {- -},(#name,initialize##name##AsmPrinter)
#include <llvm/Config/AsmPrinters.def>
                  ])
    mp2 = foldl (\mp (name,fun) -> Map.adjust (\ti -> ti { initializeAsmParser = Just fun }
                                              ) name mp) mp1
            (tail [undefined
#define LLVM_ASM_PARSER(name) {- -},(#name,initialize##name##AsmParser)
#include <llvm/Config/AsmParsers.def>
                  ])
    mp3 = foldl (\mp (name,fun) -> Map.adjust (\ti -> ti { initializeDisassembler = Just fun }
                                              ) name mp) mp2
            (tail [undefined
#define LLVM_DISASSEMBLER(name) {- -},(#name,initialize##name##Disassembler)
#include <llvm/Config/Disassemblers.def>
                  ])

foreign import capi "LLVMLinkInInterpreter"
  linkInInterpreter :: IO ()

foreign import capi "LLVMLinkInJIT"
  linkInJIT :: IO ()
