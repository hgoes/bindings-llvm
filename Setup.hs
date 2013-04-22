module Main where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Simple.Program
import Distribution.Verbosity
import qualified Distribution.ModuleName as M
import System.Process
import Data.Version
import Text.ParserCombinators.ReadP
import Data.List (find,intersperse)
import System.FilePath
import System.Directory
import Generator
import LLVMSpec

llvmConfigProgram :: Program
llvmConfigProgram = simpleProgram "llvm-config"

adaptHooks :: UserHooks -> UserHooks
adaptHooks hooks
  = hooks { hookedPrograms = llvmConfigProgram:hookedPrograms hooks
          , confHook = \pd flags -> do
            let db1 = userSpecifyPaths (configProgramPaths flags) (configPrograms flags)
                db2 = userSpecifyArgss (configProgramArgs flags) db1
            db3 <- configureProgram normal llvmConfigProgram db2
            lbi <- confHook hooks pd (flags { configPrograms = db3 })
            adaptLocalBuildInfo lbi
          , buildHook = \pd lbi uh bf -> do
            createDirectoryIfMissing True (buildDir lbi </> "wrapper")
            createDirectoryIfMissing True (buildDir lbi </> "LLVM" </> "FFI")
            version <- getLLVMVersion (configPrograms $ configFlags lbi)
            writeWrapper "HS_LLVM_PROXY" (llvm version)
              (buildDir lbi)
              proxy_h
              wrap_c
              iface_m
            buildHook hooks
              (pd { library = case library pd of
                       Just lib -> Just $
                                   lib { libBuildInfo = (libBuildInfo lib)
                                                        { cSources = [buildDir lbi </> wrap_c]
                                                                     ++(cSources (libBuildInfo lib))
                                                        }
                                       }
                  }) lbi uh bf
          }
  where
    proxy_h = "wrapper" </> "llvm_proxy.h"
    wrap_c = "wrapper" </> "llvm_wrap.cxx"
    iface_m = ["LLVM","FFI","Interface"]
    iface_name = concat $ intersperse "." iface_m

main = defaultMainWithHooks $
       adaptHooks simpleUserHooks

adaptLocalBuildInfo :: LocalBuildInfo -> IO LocalBuildInfo
adaptLocalBuildInfo bi = do
  let db = configPrograms $ configFlags bi
  version <- getLLVMVersion db
  cflags <- getLLVMCFlags db
  libs <- getLLVMLibs db
  libdir <- getLLVMLibdir db
  incdir <- getLLVMIncludedir db
  return $ bi { localPkgDescr = adaptPackageDescription 
                                (localPkgDescr bi)
                                version cflags 
                                libs libdir incdir }

adaptPackageDescription :: PackageDescription -> Version -> [String] -> [String] -> String -> String -> PackageDescription
adaptPackageDescription pkg vers cflags ldflags libdir incdir
  = case library pkg of
    Just lib 
      -> pkg { library = Just $ 
                         lib { libBuildInfo = adaptBuildInfo 
                                              (libBuildInfo lib) 
                                              vers cflags 
                                              ldflags libdir incdir
                             }
             , description = (description pkg) ++ "\n%LLVM_VERSION="++show vers++"%"
             }

adaptBuildInfo :: BuildInfo -> Version -> [String] -> [String] -> String -> String -> BuildInfo
adaptBuildInfo bi vers cflags libs libdir incdir
  = bi { cppOptions = ["-DHS_LLVM_VERSION="++versionToDefine vers]++
                      cppOptions bi
       , ccOptions = cflags++
                     ["-DHS_LLVM_VERSION="++versionToDefine vers]++
                     ccOptions bi
       , includeDirs = incdir:includeDirs bi
       , extraLibs = libs++extraLibs bi
       , extraLibDirs = libdir:extraLibDirs bi
       }

getLLVMVersion :: ProgramConfiguration -> IO Version
getLLVMVersion db = do
  outp <- getDbProgramOutput normal llvmConfigProgram db ["--version"]
  let parses = readP_to_S parseVersion outp
  case find (\(vers,rest) -> rest == "\n" || rest == "svn\n") parses of
    Just (version,_) -> return version
    Nothing -> error $ "Failed to parse llvm version: "++show parses

getLLVMCFlags :: ProgramConfiguration -> IO [String]
getLLVMCFlags db = do
  outp <- getDbProgramOutput normal llvmConfigProgram db ["--cxxflags"]
  return $ words outp

getLLVMLibdir :: ProgramConfiguration -> IO String
getLLVMLibdir db = do
  outp <- getDbProgramOutput normal llvmConfigProgram db ["--libdir"]
  let [dir] = lines outp
  return dir

getLLVMLibs :: ProgramConfiguration -> IO [String]
getLLVMLibs db = do
  outp <- getDbProgramOutput normal llvmConfigProgram db ["--libnames"]
  return $ fmap (\lib -> case dropExtension lib of
                    'l':'i':'b':name -> name
                    name -> name
                ) (words outp)

getLLVMIncludedir :: ProgramConfiguration -> IO String
getLLVMIncludedir db = do
  outp <- getDbProgramOutput normal llvmConfigProgram db ["--includedir"]
  let [dir] = lines outp
  return dir

versionToDefine :: Version -> String
versionToDefine v = branch (versionBranch v)
  where
    branch (x:xs) = show x ++ (concat $ 
                               fmap (\x' -> if x'<10
                                            then "0"++show x'
                                            else show x'
                                    ) xs)