module Main where

import LLVM.FFI

import ExampleModule
import ExampleExecution

main = withContext $ \ctx -> do
  (mod,fun) <- createExampleModule ctx
  executeExampleFunction False mod fun 32 >>= print
