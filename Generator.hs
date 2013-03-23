module Generator where

import CPPType
import Data.List
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Data.Char
import System.FilePath
import Data.Ord

data ClassSpec
  = ClassSpec { cspecHeader :: String
              , cspecNS :: NS
              , cspecClassName :: String
              , cspecTemplateArgs :: [Type]
              , cspecFunctions :: [(FunSpec,GenSpec,String)]
              }

data GenSpec = GenOnlyC
             | GenHS

className :: ClassSpec -> String
className cs = renderNS (cspecNS cs) ++
               cspecClassName cs ++
               renderTempl (cspecTemplateArgs cs)

classType :: ClassSpec -> Type
classType cs = Type [] (NamedType (cspecNS cs) (cspecClassName cs) (cspecTemplateArgs cs))

data FunSpec = Constructor { ftConArgs :: [(Bool,Type)]
                           }
             | Destructor { ftOverloadedDestructor :: Bool }
             | MemberFun { ftReturnType :: Type
                         , ftName :: String
                         , ftTemplArgs :: [Type]
                         , ftArgs :: [(Bool,Type)]
                         , ftStatic :: Bool
                         , ftOverloaded :: Bool
                         , ftPure :: Bool
                         }

type OutConverter = String -> ([String],String)
type InConverter = String -> ([String],String)

memberFun :: FunSpec
memberFun = MemberFun { ftReturnType = normalT void
                      , ftName = ""
                      , ftTemplArgs = []
                      , ftArgs = []
                      , ftStatic = False
                      , ftOverloaded = False
                      , ftPure = False
                      }

idOut :: OutConverter
idOut x = ([],x)

idIn :: InConverter
idIn x = ([],x)

copyOut :: Type -> OutConverter
copyOut tp x = ([],"new "++renderType tp++"("++x++")")

passAsPointer :: Type -> InConverter
passAsPointer tp x = ([],"*(("++renderType tp++"*)"++x++")")

refToPtr :: Type -> OutConverter
refToPtr tp x = ([],"("++(renderType tp)++"*) &"++x)

ptrToRef :: Type -> InConverter
ptrToRef tp x = ([],"*("++x++")")

voidCastOut :: OutConverter
voidCastOut x = ([],"(void*)("++x++")")

voidCastIn :: Type -> InConverter
voidCastIn tp x = ([],"("++renderType tp++"*)("++x++")")

renderType :: Type -> String
renderType (Type qual tp) 
  = concat (fmap (\q -> renderQualifier q++" ") qual)
    ++ renderC tp
  where
    renderQualifier QConst = "const"
    renderC (NamedType ns str templ) = renderNS ns ++ str ++ renderTempl templ
    renderC (PtrType tp) = renderC tp++"*"
    renderC (RefType tp) = renderC tp++"&"

renderNS :: NS -> String
renderNS = concat . fmap (\ns -> ns++"::")

renderTempl :: [Type] -> String
renderTempl [] = ""
renderTempl xs = "<"++concat (fmap renderType xs)++">"

normalT :: TypeC -> Type
normalT = Type []

constT :: TypeC -> Type
constT = Type [QConst]

char = NamedType [] "char" []
void = NamedType [] "void" []
size_t = NamedType [] "size_t" []
bool = NamedType [] "bool" []
unsigned = NamedType [] "unsigned" []
int = NamedType [] "int" []
uint64_t = NamedType [] "uint64_t" []
ptr = PtrType
ref = RefType
llvmType name = NamedType ["llvm"] name []

toPtr :: Type -> Type
toPtr (Type qual tp) = Type qual (ptr tp)

toConstRef :: Type -> Type
toConstRef (Type _ tp) = Type [QConst] (RefType tp)

toConstPtr :: Type -> Type
toConstPtr (Type _ tp) = Type [QConst] (PtrType tp)


cstring = PtrType char

isCType :: TypeC -> Bool
isCType (NamedType [] name []) = case name of
  "void" -> True
  "char" -> True
  "size_t" -> True
  "int" -> True
  "int64_t" -> True
  "uint64_t" -> True
  "bool" -> True
  "unsigned" -> True
  _ -> False
isCType (PtrType tp) = isCType tp
isCType _ = False

toCType :: Type -> (Type,OutConverter,InConverter)
toCType (Type q c) = let (x,out,inC) = toCType' c
                     in (Type q x,out,inC)
  where
    toCType' (RefType t) = let (tp',outC',inC') = toCType' (PtrType t)
                           in (tp',\x -> let (out1,r1) = refToPtr (Type q t) x
                                             (out2,r2) = outC' r1
                                         in (out1++out2,r2),
                               \x -> let (out1,r1) = inC' x
                                         (out2,r2) = ptrToRef (Type q t) r1 
                                     in (out1++out2,r2))
    toCType' (PtrType t) = if isCType t
                           then (PtrType t,idOut,idIn)
                           else (PtrType void,voidCastOut,voidCastIn (Type q t))
    toCType' t = if isCType t
                 then (t,idOut,idIn)
                 else (ptr void,copyOut (Type q t),passAsPointer (Type q t))

toHaskellType :: Bool -> Bool -> Type -> HsType
toHaskellType _ True _
  = HsTyApp (HsTyCon $ UnQual $ HsIdent "Ptr") (HsTyVar $ HsIdent "t")
toHaskellType addP False (Type q c) = toHSType (not addP) c
  where
    toHSType _ (RefType t) = HsTyApp 
                             (HsTyCon $ UnQual $ HsIdent "Ptr")
                             (toHSType True t)
    toHSType _ (PtrType t) = HsTyApp 
                             (HsTyCon $ UnQual $ HsIdent "Ptr")
                             (toHSType True t)
    toHSType isP (NamedType _ name []) = case name of
      "void" -> HsTyTuple []
      "char" -> HsTyCon $ UnQual $ HsIdent "CChar"
      "size_t" -> HsTyCon $ UnQual $ HsIdent "CSize"
      "int" -> HsTyCon $ UnQual $ HsIdent "CInt"
      "int64_t" -> HsTyCon $ UnQual $ HsIdent "Int64"
      "uint64_t" -> HsTyCon $ UnQual $ HsIdent "Word64"
      "bool" -> HsTyCon $ UnQual $ HsIdent "Bool"
      "unsigned" -> HsTyCon $ UnQual $ HsIdent "CUInt"
      _ -> (if isP
            then id 
            else HsTyApp (HsTyCon $ UnQual $ HsIdent "Ptr")
           ) $ HsTyCon $ UnQual $ HsIdent $ hsName name
    toHSType isP (NamedType ns name tmpl) 
      = (if isP
         then id
         else HsTyApp (HsTyCon $ UnQual $ HsIdent "Ptr")
        ) $ foldl HsTyApp (toHSType True (NamedType ns name [])) (fmap (toHaskellType False False) tmpl)

writeWrapper :: String -> [ClassSpec] -> String -> String -> String -> [String] -> IO ()
writeWrapper inc_sym spec build_path header_f wrapper_f ffi_f = do
  let (hcont,wcont) = generateWrapper inc_sym spec
  writeFile (build_path </> header_f) hcont
  writeFile (build_path </> wrapper_f) wcont
  writeFile (build_path </> joinPath ffi_f <.> "hs") 
    (generateFFI ffi_f header_f spec)

generateWrapper :: String -> [ClassSpec] -> (String,String)
generateWrapper inc_sym spec
  = let includes = ["#include <"++cs++">" | cs <- nub $ fmap cspecHeader spec]
        all_cont = concat [ fmap (generateWrapperFunction cs) (cspecFunctions cs)
                          | cs <- spec ]
        header_cont = unlines $ ["#ifndef "++inc_sym
                                ,"#define "++inc_sym
                                ,"#include <stdint.h>"
                                ,"#include <stdlib.h>"
                                ,"#include <stdbool.h>"] ++
                      concat (fmap fst all_cont) ++ ["#endif"]
        wrapper_cont = unlines $ includes ++ ["extern \"C\" {"] ++ 
                       concat (fmap snd all_cont) ++ ["}"]
    in (header_cont,wrapper_cont)
  where
    generateWrapperFunction :: ClassSpec -> (FunSpec,GenSpec,String) -> ([String],[String])
    generateWrapperFunction cls (fun,_,as)
      = let args = case fun of
              Constructor args -> mkArgs $ fmap snd args
              Destructor _ -> [(self_ptr,"self")]
              MemberFun { ftArgs = args 
                        , ftStatic = stat } -> (if stat
                                                then id
                                                else ((self_ptr,"self"):)) (mkArgs $ fmap snd $ args)
            self_ptr = toPtr (classType cls)
            (args',cmds) = unzip $ fmap (\(tp,name) -> let (tp',_,inC) = toCType tp
                                                           (cmds,res) = inC name
                                                       in ((tp',name,res),cmds)
                                        ) args
            rt = case fun of
              Constructor _ -> normalT $ ptr void
              Destructor _ -> normalT void
              MemberFun { ftReturnType = tp } -> tp
            (rt',outC,_) = toCType rt
            sig = renderType rt'++" "++as++"("++(paramList [ (tp,name) | (tp,name,_) <- args'])++")"
            body = case fun of
              Constructor _ -> ["  return (void*) new "++className cls++"("++argList [ (tp,name) | (tp,_,name) <- args' ]++");"]
              Destructor _ -> ["  "++className cls++"* rself = ("++className cls++"*)self;"
                            ,"  delete rself;"]
              MemberFun { ftName = name 
                        , ftStatic = stat 
                        , ftTemplArgs = tmpl } 
                -> let rself = "(("++className cls++"*)self)"
                       rargs = if stat
                               then args'
                               else tail args'
                       targs = case tmpl of
                         [] -> ""
                         _ -> "<"++concat (intersperse "," (fmap renderType tmpl))++">"
                       (conv,res) = outC $ (if stat
                                            then className cls ++ "::"
                                            else rself++"->")++name++targs++
                                    "("++argList [(tp,name) | (tp,_,name) <- rargs]++")"
                   in conv++
                      ["  return "++res++";"]
        in ([sig++";"],[sig++" {"]++concat cmds++body++["}"])

generateFFI :: [String] -> String -> [ClassSpec] -> String
generateFFI mname header specs 
  = unlines (["module "++concat (intersperse "." mname)++" where"
             ,""
             ,"import Foreign"
             ,"import Foreign.C"
             ,""]++dts++fns)
  where
    dts = [ "data "++hsName (cspecClassName cs) ++
            concat (fmap (\(_,i) -> " a"++show i) (zip (cspecTemplateArgs cs) [0..]))++
            " = "++hsName (cspecClassName cs)
          | cs <- nubBy (\x y -> cspecClassName x == cspecClassName y) specs
          ]
    fns = concat [ [""
                   ,"foreign import capi \""++header++" "++c_name++"\""
                   ,"  "++c_name++" :: "++sig]
                 | cs <- specs
                 , (fun,GenHS,c_name) <- cspecFunctions cs
                 , let sig = (concat [ prettyPrint tp ++ " -> " 
                                     | tp <- case fun of
                                       MemberFun { ftArgs = r 
                                                 , ftStatic = False 
                                                 , ftOverloaded = isO } -> (toHaskellType True isO $toPtr $ classType cs)
                                                                           :fmap (uncurry (toHaskellType True)) r
                                       MemberFun { ftArgs = r
                                                 , ftStatic = True } -> fmap (uncurry (toHaskellType True)) r
                                       Constructor r -> fmap (uncurry $ toHaskellType True) r
                                       Destructor over -> [toHaskellType True over $ toPtr $ classType cs]
                                     ]) ++
                             prettyPrint ((case fun of
                                             MemberFun { ftPure = True }
                                               -> id
                                             _ -> HsTyApp (HsTyCon $ UnQual $ HsIdent "IO"))
                                          (toHaskellType True False $ case fun of
                                              MemberFun { ftReturnType = t } -> t
                                              Constructor _ -> toPtr $ classType cs
                                              Destructor _ -> normalT void
                                          ))
                 ]

hsName :: String -> String
hsName (c:cs) = if isLower c
                then toUpper c:cs
                else c:cs

mkArgs :: [Type] -> [(Type,String)]
mkArgs tps = fmap (\(tp,i) -> (tp,"arg"++show i)) (zip tps [0..])

paramList :: [(Type,String)] -> String
paramList = comma . fmap (\(tp,name) -> renderType tp++" "++name)

comma :: [String] -> String
comma = concat . intersperse ","

argList :: [(Type,String)] -> String
argList = comma . fmap (\(_,arg) -> arg)