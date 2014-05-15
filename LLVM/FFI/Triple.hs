module LLVM.FFI.Triple
       (Triple()
       ,ArchType(..)
       ,VendorType(..)
       ,OSType(..)
#if HS_LLVM_VERSION>=209
       ,EnvironmentType(..)
#endif
#if HS_LLVM_VERSION>304
       ,ObjectFormatType(..)
#endif
       ,newTripleEmpty
       ,newTripleFromString
       ,tripleGetArch
       ,tripleGetVendor
       ,tripleGetOS
       ,tripleHasEnvironment
#if HS_LLVM_VERSION>=209
       ,tripleGetEnvironment
#endif
#if HS_LLVM_VERSION>=300
       ,tripleGetOSVersion
#endif
#if HS_LLVM_VERSION>=301
       ,tripleGetMacOSXVersion
#endif
#if HS_LLVM_VERSION>=302
       ,tripleGetiOSVersion
#endif
       ,tripleGetArchName
       ,tripleGetVendorName
       ,tripleGetOSName
       ,tripleGetEnvironmentName
#if HS_LLVM_VERSION>=301
       ,tripleIsArch16Bit
       ,tripleIsArch32Bit
       ,tripleIsArch64Bit
#endif
#if HS_LLVM_VERSION>=300
       ,tripleIsMacOSX
#endif
#if HS_LLVM_VERSION>=303
       ,tripleIsiOS
#endif
#if HS_LLVM_VERSION>=300
       ,tripleIsOSDarwin
#endif
#if HS_LLVM_VERSION>304
       ,tripleIsOSFreeBSD
       ,tripleIsWindowsMSVCEnvironment
       ,tripleIsWindowsCygwinEnvironment
       ,tripleIsWindowsGNUEnvironment
       ,tripleIsOSCygMing
       ,tripleIsOSMSVCRT
#endif
#if HS_LLVM_VERSION>=300
       ,tripleIsOSWindows
#endif
#if HS_LLVM_VERSION>=303
       ,tripleIsOSNaCl
#endif
#if HS_LLVM_VERSION>304
       ,tripleIsOSLinux
#endif
       ,tripleSetArch
       ,tripleSetVendor
       ,tripleSetOS
#if HS_LLVM_VERSION>=209
       ,tripleSetEnvironment
#endif
       ) where

import LLVM.FFI.Interface
import Foreign
import Foreign.C

tripleGetArch :: Ptr Triple -> IO ArchType
tripleGetArch = fmap toArchType . tripleGetArch_

tripleGetVendor :: Ptr Triple -> IO VendorType
tripleGetVendor = fmap toVendorType . tripleGetVendor_

tripleGetOS :: Ptr Triple -> IO OSType
tripleGetOS = fmap toOSType . tripleGetOS_

#if HS_LLVM_VERSION>=209
tripleGetEnvironment :: Ptr Triple -> IO EnvironmentType
tripleGetEnvironment = fmap toEnvironmentType . tripleGetEnvironment_
#endif

#if HS_LLVM_VERSION>=300
tripleGetOSVersion :: Ptr Triple -> IO (CUInt,CUInt,CUInt)
tripleGetOSVersion t
  = alloca $
    \pmajor
    -> alloca $
       \pminor
        -> alloca $
           \pmicro -> do
             tripleGetOSVersion_ t pmajor pminor pmicro
             major <- peek pmajor
             minor <- peek pminor
             micro <- peek pmicro
             return (major,minor,micro)
#endif

#if HS_LLVM_VERSION>=301
tripleGetMacOSXVersion :: Ptr Triple -> IO (Maybe (CUInt,CUInt,CUInt))
tripleGetMacOSXVersion t
  = alloca $
    \pmajor
    -> alloca $
       \pminor
        -> alloca $
           \pmicro -> do
             res <- tripleGetMacOSXVersion_ t pmajor pminor pmicro
             if res
               then (do
                        major <- peek pmajor
                        minor <- peek pminor
                        micro <- peek pmicro
                        return $ Just (major,minor,micro))
               else return Nothing
#endif

#if HS_LLVM_VERSION>=302
tripleGetiOSVersion :: Ptr Triple -> IO (CUInt,CUInt,CUInt)
tripleGetiOSVersion t
  = alloca $
    \pmajor
    -> alloca $
       \pminor
        -> alloca $
           \pmicro -> do
             tripleGetiOSVersion_ t pmajor pminor pmicro
             major <- peek pmajor
             minor <- peek pminor
             micro <- peek pmicro
             return (major,minor,micro)
#endif

tripleSetArch :: Ptr Triple -> ArchType -> IO ()
tripleSetArch t arch = tripleSetArch_ t (fromArchType arch)

tripleSetVendor :: Ptr Triple -> VendorType -> IO ()
tripleSetVendor t vendor = tripleSetVendor_ t (fromVendorType vendor)

tripleSetOS :: Ptr Triple -> OSType -> IO ()
tripleSetOS t os = tripleSetOS_ t (fromOSType os)

#if HS_LLVM_VERSION>=209
tripleSetEnvironment :: Ptr Triple -> EnvironmentType -> IO ()
tripleSetEnvironment t env = tripleSetEnvironment_ t (fromEnvironmentType env)
#endif
