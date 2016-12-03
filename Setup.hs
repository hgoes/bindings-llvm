module Main where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.PackageDescription
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Verbosity
import qualified Distribution.ModuleName as M
import System.Process
import System.Exit
import System.IO
import Data.Version
import Text.ParserCombinators.ReadP
import Data.List (find,intersperse)
import System.FilePath
import System.Directory
import Generator
import LLVMSpec
import Data.Maybe (catMaybes)
import Distribution.Compat.Semigroup

llvmConfigProgram :: Program
llvmConfigProgram = simpleProgram "llvm-config"

adaptHooks :: UserHooks -> UserHooks
adaptHooks hooks
  = hooks { hookedPrograms = llvmConfigProgram:cppProgram:hookedPrograms hooks
          , confHook = \pd flags -> do
            let db0 = case getLast' (configPrograms_ flags) of
                  Nothing -> defaultProgramDb
                  Just db -> db
                db1 = userSpecifyPaths (configProgramPaths flags) db0
                db2 = userSpecifyArgss (configProgramArgs flags) db1
            db3 <- configureProgram normal llvmConfigProgram db2
            lbi <- confHook hooks pd (flags { configPrograms_ = Last' $ Just db3 })
            let shared = elem (FlagName "shared-llvm",True) (configConfigurationsFlags flags)
            adaptLocalBuildInfo shared lbi
          , buildHook = \pd lbi uh bf -> do
            createDirectoryIfMissing True (buildDir lbi </> "wrapper")
            createDirectoryIfMissing True (buildDir lbi </> "LLVM" </> "FFI")
            buildHook hooks
              (pd { library = case library pd of
                       Just lib -> Just $
                                   lib { libBuildInfo = (libBuildInfo lib)
                                                        { cSources = [buildDir lbi </> wrap_c]
                                                                     ++(cSources (libBuildInfo lib))
                                                        }
                                       }
                  }) lbi uh bf
          , hookedPreProcessors = ("llvm-spec",prep):hookedPreProcessors hooks
          }
  where
    proxy_h = "wrapper" </> "llvm_proxy.h"
    wrap_c = "wrapper" </> "llvm_wrap.cxx"
    iface_m = ["LLVM","FFI","Interface"]
    iface_name = concat $ intersperse "." iface_m
    prep bi lbi = PreProcessor { platformIndependent = True
                               , runPreProcessor = mkSimplePreProcessor $ \infile outfile verbosity -> do
                                   version <- getLLVMVersion (configPrograms $ configFlags lbi)
                                   modTime <- getModificationTime "LLVMSpec.hs"
                                   writeWrapper "HS_LLVM_PROXY" (llvm version)
                                     (buildDir lbi)
                                     modTime
                                     proxy_h
                                     wrap_c
                                     iface_m
                               }

main = defaultMainWithHooks $
       adaptHooks simpleUserHooks

adaptLocalBuildInfo :: Bool -> LocalBuildInfo -> IO LocalBuildInfo
adaptLocalBuildInfo shared bi = do
  let db = configPrograms $ configFlags bi
  version <- getLLVMVersion db
  cflags <- getLLVMCFlags db >>= filterCFlags db
  cppflags <- getLLVMCppFlags db
  ldflags <- getLLVMLDFlags version db
  libs <- if shared
          then return ["LLVM-"++showVersion version]
          else getLLVMLibs db
  libdir <- getLLVMLibdir db
  incdir <- getLLVMIncludedir db
  return $ bi { localPkgDescr = adaptPackageDescription 
                                (localPkgDescr bi)
                                version cflags ldflags cppflags
                                libs libdir incdir }

adaptPackageDescription :: PackageDescription -> Version -> [String] -> [String] -> [String] -> [String] -> String -> String -> PackageDescription
adaptPackageDescription pkg vers cflags ldflags cppflags libs libdir incdir
  = case library pkg of
    Just lib 
      -> pkg { library = Just $ 
                         lib { libBuildInfo = adaptBuildInfo 
                                              (libBuildInfo lib) 
                                              vers cflags 
                                              ldflags cppflags
                                              libs libdir incdir
                             }
             , testSuites = fmap (\ts -> ts { testBuildInfo = adaptBuildInfo
                                                              (testBuildInfo ts)
                                                              vers cflags
                                                              ldflags cppflags
                                                              libs libdir incdir
                                            }) (testSuites pkg)
             , description = (description pkg) ++ "\n%LLVM_VERSION="++show vers++"%"
             }

adaptBuildInfo :: BuildInfo -> Version -> [String] -> [String] -> [String] -> [String] -> String -> String -> BuildInfo
adaptBuildInfo bi vers cflags ldflags cppflags libs libdir incdir
  = bi { cppOptions = ["-DHS_LLVM_VERSION="++versionToDefine vers]++
                      cppflags++
                      ["-I/usr/include"]++
                      cppOptions bi
       , ccOptions = cflags++
                     {-(if vers >= Version [3,0,0] []
                      then ["-std=c++11"]
                      else [])++-}
                     ["-DHS_LLVM_VERSION="++versionToDefine vers]++
                     ccOptions bi
       , ldOptions = ldflags++(ldOptions bi)
       , includeDirs = incdir:includeDirs bi
       , extraLibs = libs++extraLibs bi
       , extraLibDirs = libdir:extraLibDirs bi
       }

runLLVMConfig :: ProgramConfiguration -> [String] -> IO String
runLLVMConfig = getDbProgramOutput normal llvmConfigProgram

getLLVMVersion :: ProgramConfiguration -> IO Version
getLLVMVersion db = do
  outp <- runLLVMConfig db ["--version"]
  let parses = readP_to_S parseVersion outp
  case find (\(vers,rest) -> rest == "\n" || rest == "svn\n") parses of
    Just (version,_) -> return version
    Nothing -> error $ "Failed to parse llvm version: "++show parses

getLLVMCFlags :: ProgramConfiguration -> IO [String]
getLLVMCFlags db = do
  outp <- runLLVMConfig db ["--cxxflags"]
  return $ words outp

getLLVMCppFlags :: ProgramConfiguration -> IO [String]
getLLVMCppFlags db = do
  outp <- runLLVMConfig db ["--cppflags"]
  return $ words outp

getLLVMLDFlags :: Version -> ProgramConfiguration -> IO [String]
getLLVMLDFlags vers db = do
  ldflags <- runLLVMConfig db ["--ldflags"]
  syslibs <- if vers >= Version [3,5,0] []
             then getDbProgramOutput normal llvmConfigProgram db ["--system-libs"]
             else return ""
  return $ words ldflags ++ words syslibs

getLLVMLibdir :: ProgramConfiguration -> IO String
getLLVMLibdir db = do
  outp <- runLLVMConfig db ["--libdir"]
  let [dir] = lines outp
  return dir

getLLVMLibs :: ProgramConfiguration -> IO [String]
getLLVMLibs db = do
  outp <- runLLVMConfig db ["--libnames"]
  return $ fmap (\lib -> case dropExtension lib of
                    'l':'i':'b':name -> name
                    name -> name
                ) (words outp)

getLLVMIncludedir :: ProgramConfiguration -> IO String
getLLVMIncludedir db = do
  outp <- runLLVMConfig db ["--includedir"]
  let [dir] = lines outp
  return dir

versionToDefine :: Version -> String
versionToDefine v = branch (versionBranch v)
  where
    branch (major:minor:_) = show major ++ (if minor < 10
                                            then "0"++show minor
                                            else show minor)

filterCFlags :: ProgramConfiguration -> [String] -> IO [String]
filterCFlags db flags = do
  (compiler,_) <- requireProgram normal cppProgram db
  let cc = locationPath $ programLocation compiler
  mapM (\flag -> do
           let args = programDefaultArgs compiler ++ ["-x","c++","-E",flag,"-"] ++ programOverrideArgs compiler
               p = (proc cc args) { env = Nothing
                                  , std_in = CreatePipe
                                  , std_out = CreatePipe
                                  , std_err = CreatePipe
                                  }
           (Just hIn,Just hOut,Just hErr,hP) <- createProcess p
           hClose hIn
           code <- waitForProcess hP
           hClose hOut
           hClose hErr
           return $ case code of
             ExitSuccess -> Just flag
             _ -> Nothing
      ) flags >>= return.catMaybes
