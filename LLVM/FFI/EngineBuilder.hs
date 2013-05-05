module LLVM.FFI.EngineBuilder
       (EngineBuilder()
       ,EngineKind(..)
       ,newEngineBuilder
       ,deleteEngineBuilder
       ,engineBuilderSetKind
       ,engineBuilderCreate
       ) where

import LLVM.FFI.Interface