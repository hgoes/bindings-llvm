module Main where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import qualified Distribution.ModuleName as M
import System.Process
import Data.Version
import Text.ParserCombinators.ReadP
import Data.List (find,intersperse)
import System.FilePath
import System.Directory
import Generator
import LLVMSpec

main = defaultMainWithHooks $
       simpleUserHooks { confHook = \cfg flags -> (confHook simpleUserHooks cfg flags) >>= adaptLocalBuildInfo 
                       , postConf = \args cfg pkgd lbi -> do
                            createDirectoryIfMissing True (buildDir lbi </> "wrapper")
                            createDirectoryIfMissing True (buildDir lbi </> "LLVM" </> "FFI")
                            version <- getLLVMVersion
                            writeWrapper "HS_LLVM_PROXY" (llvm version)
                              (buildDir lbi)
                              proxy_h
                              wrap_c
                              iface_m
                       , buildHook = \pd lbi uh bf -> do
                            buildHook simpleUserHooks 
                              (pd { library = case library pd of
                                       Just lib -> Just $ lib { libBuildInfo = (libBuildInfo lib) 
                                                                               { cSources = [buildDir lbi </> wrap_c]
                                                                                            ++(cSources (libBuildInfo lib))
                                                                               --, otherModules = [M.fromString iface_name]
                                                                               --                 ++(otherModules (libBuildInfo lib))
                                                                               }
                                                              } 
                                  })
                              lbi uh bf
                       }
  where
    proxy_h = "wrapper" </> "llvm_proxy.h"
    wrap_c = "wrapper" </> "llvm_wrap.cxx"
    iface_m = ["LLVM","FFI","Interface"]
    iface_name = concat $ intersperse "." iface_m
                                         
adaptLocalBuildInfo :: LocalBuildInfo -> IO LocalBuildInfo
adaptLocalBuildInfo bi = do
  version <- getLLVMVersion
  cflags <- getLLVMCFlags
  libs <- getLLVMLibs
  libdir <- getLLVMLibdir
  incdir <- getLLVMIncludedir
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

getLLVMVersion :: IO Version
getLLVMVersion = do
  outp <- readProcess "llvm-config" ["--version"] ""
  let parses = readP_to_S parseVersion outp
  case find (\(vers,rest) -> rest == "\n" || rest == "svn\n") parses of
    Just (version,_) -> return version
    Nothing -> error $ "Failed to parse llvm version: "++show parses

getLLVMCFlags :: IO [String]
getLLVMCFlags = do
  outp <- readProcess "llvm-config" ["--cflags"] ""
  return $ words outp

getLLVMLibdir :: IO String
getLLVMLibdir = do
  outp <- readProcess "llvm-config" ["--libdir"] ""
  let [dir] = lines outp
  return dir

getLLVMLibs :: IO [String]
getLLVMLibs = do
  outp <- readProcess "llvm-config" ["--libnames"] ""
  return $ fmap (\lib -> case dropExtension lib of
                    'l':'i':'b':name -> name
                    name -> name
                ) (words outp)

getLLVMIncludedir :: IO String
getLLVMIncludedir = do
  outp <- readProcess "llvm-config" ["--includedir"] ""
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