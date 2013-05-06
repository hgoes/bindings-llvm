module LLVM.FFI.TargetMachine
       (TargetMachine()
       ,TargetMachineC()
       ,deleteTargetMachine
       ,targetMachineTarget
       ) where

import LLVM.FFI.Interface
import Foreign.Ptr

class TargetMachineC t

instance TargetMachineC TargetMachine

deleteTargetMachine :: TargetMachineC t => Ptr t -> IO ()
deleteTargetMachine = deleteTargetMachine_

targetMachineTarget :: TargetMachineC t => Ptr t -> IO (Ptr Target)
targetMachineTarget = targetMachineTarget_