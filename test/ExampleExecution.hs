module ExampleExecution where

import LLVM.FFI
import Foreign
import Foreign.C.String

executeExampleFunction :: Bool -> Ptr Module -> Ptr Function -> Int -> IO Int
executeExampleFunction jit mod fun v = do
  if jit then linkInJIT
    else linkInInterpreter
  if jit then case nativeTarget of
    Nothing -> error "No native target available!"
    Just trg -> do
      initializeTargetInfo trg
      initializeTarget trg
      initializeTargetMC trg
      return ()
    else return ()
#if HS_LLVM_VERSION<306
  builder <- newEngineBuilder mod
#else
  ref <- newUniquePtr mod
  builder <- newEngineBuilder ref
  deleteUniquePtr ref
#endif
  engineBuilderSetKind builder (if jit then JIT else Interpreter)
  errStr <- cppStringEmpty
  engineBuilderSetErrorStr builder errStr
  putStrLn "Creating engine"
  engine <- engineBuilderCreate builder
  if engine==nullPtr
    then (do
             cerrStr <- cppStringToString errStr
             err <- peekCString cerrStr
             putStrLn err
             error "Engine not created!")
    else return ()
  putStrLn "Creating APInt"
  argAP <- newAPIntLimited 32 (fromIntegral v) True
  putStrLn "Creating GenericValue"
  arg <- newGenericValue
  putStrLn "Setting GenericValue"
  genericValueSetInt arg argAP
  putStrLn "Creating argument vector"
  argVec <- newVector arg (advancePtr arg 1)
  putStrLn "Running function"
  rval <- executionEngineRunFunction engine fun argVec
  putStrLn "Extracting result"
  rval' <- genericValueGetInt rval
  putStrLn "Extracting value"
  rval'' <- apIntGetSExtValue rval'
  return $ fromIntegral rval''
