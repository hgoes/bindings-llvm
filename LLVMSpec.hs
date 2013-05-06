module LLVMSpec where

import Data.Version
import Generator
import CPPType

llvm3_3 :: Version
llvm3_3 = Version { versionBranch = [3,3]
                  , versionTags = []
                  }

llvm3_2 :: Version
llvm3_2 = Version { versionBranch = [3,2]
                  , versionTags = []
                  }

llvm3_1 :: Version
llvm3_1 = Version { versionBranch = [3,1]
                  , versionTags = []
                  }

llvm3_0 :: Version
llvm3_0 = Version { versionBranch = [3,0]
                  , versionTags = []
                  }

llvm2_9 :: Version
llvm2_9 = Version { versionBranch = [2,9]
                  , versionTags = []
                  }

irInclude :: Version -> String -> String
irInclude ver hdr = if ver >= llvm3_3
                    then "llvm/IR/"++hdr
                    else "llvm/"++hdr

llvm :: Version -> [Spec]
llvm version
  = [Spec { specHeader = "llvm/ADT/StringRef.h"
          , specNS = llvmNS
          , specName = "StringRef"
          , specTemplateArgs = []
          , specType = ClassSpec
                       [(Constructor { ftConArgs = [] },GenHS,"newStringRefEmpty")
                       ,(Constructor { ftConArgs = [(False,constT cstring)] },GenHS,"newStringRef_")
                       ,(Destructor False,GenHS,"deleteStringRef")
                       ,(memberFun { ftReturnType = constT cstring
                                   , ftName = "data"
                                   },GenHS,"stringRefData_")
                       ,(memberFun { ftReturnType = normalT size_t
                                   , ftName = "size"
                                   },GenHS,"stringRefSize_")
                       ]
          }
    ]++
    [Spec { specHeader = "llvm/ADT/OwningPtr.h"
          , specNS = llvmNS
          , specName = "OwningPtr"
          , specTemplateArgs = [rtp]
          , specType = ClassSpec
                       [(Constructor { ftConArgs = [(False,toPtr rtp)] },GenHS,"newOwningPtr"++tp)
                       ,(Destructor False,GenHS,"deleteOwningPtr"++tp)
                       ,(memberFun { ftReturnType = toPtr rtp
                                   , ftName = "take"
                                   },GenHS,"takeOwningPtr"++tp)]
          }
     | tp <- ["MemoryBuffer"]
    , let rtp = Type [] (NamedType llvmNS tp [])
    ]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/ADT/ArrayRef.h"
                , specNS = llvmNS
                , specName = "ArrayRef"
                , specTemplateArgs = [rtp]
                , specType = ClassSpec $
                             [(Constructor { ftConArgs = [] },GenHS,"newArrayRefEmpty"++tp)
                             ,(Constructor { ftConArgs = [(False,toPtr rtp)
                                                         ,(False,normalT size_t)] },GenHS,"newArrayRef"++tp)
                             ,(Destructor False,GenHS,"deleteArrayRef"++tp)
                             ,(memberFun { ftReturnType = normalT size_t
                                         , ftName = "size"
                                         },GenHS,"arrayRefSize"++tp)]++
                             (if version>=llvm3_0
                              then [(memberFun { ftReturnType = normalT bool
                                               , ftName = "equals"
                                               , ftArgs = [(False,normalT $ NamedType llvmNS "ArrayRef" [rtp])]
                                               },GenHS,"arrayRefEquals"++tp)]
                              else [])++
                             [(memberFun { ftReturnType = toConstRef rtp
                                         , ftName = "operator[]"
                                         , ftArgs = [(False,normalT size_t)]
                                         },GenHS,"arrayRefIndex"++tp)
                             ]
                }
           | (tp,rtp) <- [("Type",normalT $ ptr $ llvmType "Type")
                        ,("CChar",constT $ ptr char)]
          ]
     else [])++
    concat [[Spec { specHeader = "llvm/ADT/ilist.h"
                  , specNS = llvmNS
                  , specName = "iplist"
                  , specTemplateArgs = [rtp]
                  , specType = ClassSpec
                               [(Constructor { ftConArgs = [] },GenHS,"new"++tp++"List")
                               ,(Destructor False,GenHS,"delete"++tp++"List")
                               ,(memberFun { ftReturnType = normalT size_t
                                           , ftName = "size"
                                           },GenHS,"list"++tp++"Size")
                               ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "ilist_iterator" [rtp]
                                           , ftName = "begin"
                                           },GenHS,"list"++tp++"Begin")
                               ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "ilist_iterator" [rtp]
                                           , ftName = "end"
                                           },GenHS,"list"++tp++"End")
                               ]
                  }
            ,Spec { specHeader = "llvm/ADT/ilist.h"
                  , specNS = llvmNS
                  , specName = "ilist_iterator"
                  , specTemplateArgs = [rtp]
                  , specType = ClassSpec
                               [(memberFun { ftReturnType = toPtr rtp
                                           , ftName = "operator->"
                                           },GenHS,"listIterator"++tp++"Deref")
                               ,(memberFun { ftReturnType = normalT $ RefType $ 
                                                            NamedType llvmNS "ilist_iterator" [rtp]
                                           , ftName = "operator++"
                                           },GenHS,"listIterator"++tp++"Next")
                               ,(memberFun { ftReturnType = normalT bool
                                           , ftName = "operator=="
                                           , ftArgs = [(False,constT $ RefType $ 
                                                             NamedType llvmNS "ilist_iterator" [rtp])] 
                                           },GenHS,"listIterator"++tp++"Eq")
                               ,(memberFun { ftReturnType = normalT bool
                                           , ftName = "operator!="
                                           , ftArgs = [(False,constT $ RefType $ 
                                                             NamedType llvmNS "ilist_iterator" [rtp])] 
                                           },GenHS,"listIterator"++tp++"NEq")]
                  }
            ]
            | tp <- ["Function","Instruction","BasicBlock","GlobalVariable","Argument"]
           , let rtp = Type [] (NamedType llvmNS tp [])
           ]++
    concat [[Spec { specHeader = "llvm/ADT/SetVector.h"
                  , specNS = llvmNS
                  , specName = "SetVector"
                  , specTemplateArgs = [rtp]
                  , specType = ClassSpec
                               [(memberFun { ftReturnType = normalT bool
                                           , ftName = "empty"
                                           },GenHS,"setVector"++tp++"Empty")
                               ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" []
                                           , ftName = "begin"
                                           },GenHS,"setVector"++tp++"Begin")
                               ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" []
                                           , ftName = "end"
                                           },GenHS,"setVector"++tp++"End")
                               ]
                  }
            ,Spec { specHeader = "vector"
                  , specNS = [ClassName "std" []]
                  , specName = "vector"
                  , specTemplateArgs = [rtp]
                  , specType = ClassSpec
                               [(memberFun { ftReturnType = constT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" []
                                           , ftName = "begin"
                                           , ftOverloaded = True
                                           },GenHS,"vector"++tp++"Begin")
                               ,(memberFun { ftReturnType = constT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" []
                                           , ftName = "end"
                                           , ftOverloaded = True
                                           },GenHS,"vector"++tp++"End")]
                  }
            ,Spec { specHeader = "vector"
                  , specNS = [ClassName "std" [],ClassName "vector" [rtp]]
                  , specName = "const_iterator"
                  , specTemplateArgs = []
                  , specType = ClassSpec
                               [(memberFun { ftReturnType = rtp
                                           , ftName = "operator*"
                                           },GenHS,"vectorIterator"++tp++"Deref")
                               ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" []
                                           , ftName = "operator++"
                                           },GenHS,"vectorIterator"++tp++"Next")
                               ]
                  }
            ,Spec { specHeader = "vector"
                  , specNS = []
                  , specName = "operator=="
                  , specTemplateArgs = []
                  , specType = GlobalFunSpec { gfunReturnType = normalT bool
                                             , gfunArgs = [(False,constT $ ref $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [])
                                                          ,(False,constT $ ref $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [])]
                                             , gfunHSName = "vectorIterator"++tp++"Eq"
                                             }
                  }
            ]
            | tp <- ["Type","Loop","BasicBlock"]
           , let rtp = Type [] (ptr $ llvmType tp)
           ]++
    [Spec { specHeader = "llvm/ADT/SmallVector.h"
          , specNS = llvmNS
          , specName = "SmallVector"
          , specTemplateArgs = [rtp,TypeInt 16]
          , specType = ClassSpec
                       [(Constructor [],GenHS,"newSmallVector"++tp)
                       ,(Destructor False,GenHS,"deleteSmallVector"++tp)
                       ,(memberFun { ftReturnType = normalT size_t
                                   , ftName = "size"
                                   , ftOverloaded = True
                                   },GenHS,"smallVectorSize"++tp)
                       ,(memberFun { ftReturnType = toPtr rtp
                                   , ftName = "data"
                                   , ftOverloaded = True
                                   },GenHS,"smallVectorData"++tp)]
          }
     | (tp,rtp) <- [("Loop",normalT $ ptr $ llvmType "Loop")
                  ,("Edge",normalT $ NamedType [ClassName "std" []] "pair" [normalT $ ptr $ llvmType "BasicBlock"
                                                                           ,normalT $ ptr $ llvmType "BasicBlock"])]
    ]++
    [Spec { specHeader = "utility"
          , specNS = [ClassName "std" []]
          , specName = "pair"
          , specTemplateArgs = [rtp1,rtp2]
          , specType = ClassSpec
                       [(Getter { ftGetType = rtp1
                                , ftGetVar = "first"
                                , ftGetStatic = False
                                },GenHS,"pairFirst"++tp1++"_"++tp2)
                       ,(Getter { ftGetType = rtp2
                                , ftGetVar = "second"
                                , ftGetStatic = False
                                },GenHS,"pairSecond"++tp1++"_"++tp2)
                       ,(SizeOf,GenHS,"sizeofPair"++tp1++"_"++tp2)
                       ]
          }
     | (tp1,tp2) <- [("BasicBlock","BasicBlock")]
    , let rtp1 = normalT $ ptr $ llvmType tp1
          rtp2 = normalT $ ptr $ llvmType tp2 ]++
       [Spec { specHeader = "llvm/ADT/APFloat.h"
             , specNS = llvmNS
             , specName = "APFloat"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT double
                                      , ftName = "convertToDouble"
                                      },GenHS,"apFloatConvertToDouble")]
             }
       ,Spec { specHeader = "llvm/ADT/APInt.h"
             , specNS = llvmNS
             , specName = "APInt"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [(False,normalT unsigned)
                                        ,(False,normalT uint64_t)
                                        ,(False,normalT bool)]
                           ,GenHS,"newAPIntLimited")
                          ,if version>=llvm3_0
                           then (Constructor [(False,normalT unsigned)
                                             ,(False,normalT $ NamedType llvmNS "ArrayRef"
                                                        [normalT uint64_t])]
                                ,GenHS,"newAPInt")
                           else (Constructor [(False,normalT unsigned)
                                             ,(False,normalT unsigned)
                                             ,(False,constT $ ptr $ uint64_t)]
                                ,GenHS,"newAPInt")
                          ,(Constructor [(False,normalT unsigned)
                                        ,(False,normalT $ llvmType "StringRef")
                                        ,(False,normalT uint8_t)]
                           ,GenHS,"newAPIntFromString")
                          ,(Destructor False,GenHS,"deleteAPInt")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getBitWidth"
                                      },GenHS,"apIntGetBitWidth")
                          ,(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getZExtValue"
                                      },GenHS,"apIntGetZExtValue")
                          ,(memberFun { ftReturnType = normalT int64_t
                                      , ftName = "getSExtValue"
                                      },GenHS,"apIntGetSExtValue")]
             }
       ,Spec { specHeader = "llvm/Support/DebugLoc.h"
             , specNS = llvmNS
             , specName = "DebugLoc"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(Constructor { ftConArgs = [] },GenHS,"newDebugLoc")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isUnknown"
                                      },GenHS,"debugLocIsUnknown")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getLine"
                                      },GenHS,"debugLocGetLine")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getCol"
                                      },GenHS,"debugLocGetCol")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "MDNode" []
                                      , ftName = "getScope"
                                      , ftArgs = [(False,constT $ ref $ NamedType llvmNS "LLVMContext" [])]
                                      },GenHS,"debugLocGetScope")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "MDNode" []
                                      , ftName = "getInlinedAt"
                                      , ftArgs = [(False,constT $ ref $ NamedType llvmNS "LLVMContext" [])]
                                      },GenHS,"debugLocGetInlinedAt")]++
                          (if version>=llvm3_0
                           then [(memberFun { ftName = "dump" 
                                            , ftArgs = [(False,constT $ ref $ NamedType llvmNS "LLVMContext" [])]
                                            },GenHS,"debugLocDump")]
                           else [])
             }
       ]++
       [Spec { specHeader = irInclude version "Type.h"
             , specNS = llvmNS
             , specName = "Type"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT void
                                      , ftName = "dump"
                                      , ftOverloaded = True
                                      },GenHS,"typeDump_")
                          ,(memberFun { ftReturnType = normalT $ ref $ llvmType "LLVMContext"
                                      , ftName = "getContext"
                                      , ftOverloaded = True
                                      },GenHS,"typeGetContext_")
                          ]++
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "is"++tp++"Ty"
                                      , ftOverloaded = True
                                      , ftPure = True
                                      },GenHS,"is"++tp++"Ty_")
                           | tp <- ["Void"]++(if version>=llvm3_1
                                             then ["Half"]
                                             else [])++
                                  ["Float","Double","X86_FP80","FP128","PPC_FP128"
                                  ,"FloatingPoint"]++
                                  (if version>=llvm2_9
                                   then ["X86_MMX"]
                                   else [])++
                                  ["Label","Metadata"]
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "IntegerType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getBitWidth"
                                      },GenHS,"getBitWidth_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "IntegerType" [] 
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ RefType $ NamedType llvmNS "LLVMContext" [])
                                                 ,(False,normalT $ NamedType [] "unsigned" [])]
                                      , ftStatic = True
                                      },GenHS,"getIntegerType_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "CompositeType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Type" []
                                      , ftName = "getTypeAtIndex"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },GenHS,"compositeTypeGetTypeAtIndex_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "indexValid"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },GenHS,"compositeTypeIndexValid_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "SequentialType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Type" []
                                      , ftName = "getElementType"
                                      , ftOverloaded = True
                                      },GenHS,"sequentialTypeGetElementType_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "ArrayType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getNumElements"
                                      },GenHS,"arrayTypeGetNumElements_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "PointerType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAddressSpace"
                                      },GenHS,"pointerTypeGetAddressSpace_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "PointerType" []
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ NamedType llvmNS "Type" [])
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },GenHS,"pointerTypeGet_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "VectorType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumElements"
                                      },GenHS,"vectorTypeGetNumElements_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "VectorType" []
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ NamedType llvmNS "Type" [])
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },GenHS,"vectorTypeGet_")
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "StructType"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isPacked"
                                      },GenHS,"structTypeIsPacked")]++
                          (if version>=llvm3_0
                           then [(memberFun { ftReturnType = normalT bool
                                            , ftName = "hasName"
                                            },GenHS,"structTypeHasName")
                                ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "StringRef" []
                                            , ftName = "getName"
                                            },GenHS,"structTypeGetName")]
                           else [])++
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumElements"
                                      },GenHS,"structTypeGetNumElements_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Type" []
                                      , ftName = "getElementType"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"structTypeGetElementType_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,if version>=llvm3_0
                                                         then normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Type"]
                                                         else constT $ ref $ NamedType [ClassName "std" []] "vector" [constT $ ptr $ llvmType "Type"])
                                                 ,(False,normalT bool)]
                                      , ftStatic = True
                                      },GenHS,"newStructType")
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = llvmNS
             , specName = "FunctionType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVarArg"
                                      },GenHS,"functionTypeIsVarArg")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumParams"
                                      },GenHS,"functionTypeGetNumParams_")
                          ,(memberFun { ftReturnType = normalT (ptr $ NamedType llvmNS "Type" [])
                                      , ftName = "getParamType"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"functionTypeGetParamType_")
                          ,(memberFun { ftReturnType = normalT (ptr $ NamedType llvmNS "Type" [])
                                      , ftName = "getReturnType"
                                      },GenHS,"functionTypeGetReturnType")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionType"
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")
                                                 ,(False,if version>=llvm3_0
                                                         then normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Type"]
                                                         else constT $ ref $ NamedType [ClassName "std" []] "vector" [constT $ ptr $ llvmType "Type"])
                                                 ,(False,normalT bool)
                                                 ]
                                      , ftStatic = True
                                      },GenHS,"newFunctionType_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Value.h"
             , specNS = llvmNS
             , specName = "Value"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor True,GenHS,"deleteValue_")
                          ,(memberFun { ftName = "dump"
                                      , ftOverloaded = True
                                      },GenHS,"valueDump_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "hasName"
                                      , ftOverloaded = True
                                      },GenHS,"hasName_")
                          ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "StringRef" []
                                      , ftName = "getName"
                                      , ftOverloaded = True
                                      },GenHS,"getName_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Type" []
                                      , ftName = "getType"
                                      , ftOverloaded = True
                                      },GenHS,"valueGetType_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Argument.h"
             , specNS = llvmNS
             , specName = "Argument"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                      , ftName = "getParent"
                                      },GenHS,"argumentGetParent")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getArgNo"
                                      },GenHS,"argumentGetArgNo_")
                          ]
             }
       ,Spec { specHeader = irInclude version "BasicBlock.h"
             , specNS = llvmNS
             , specName = "BasicBlock"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Function" []
                                      , ftName = "getParent"
                                      },GenHS,"basicBlockGetParent")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType llvmNS "iplist" 
                                                       [normalT $ NamedType llvmNS "Instruction" []]
                                      , ftName = "getInstList"
                                      },GenHS,"getInstList")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "TerminatorInst" []
                                      , ftName = "getTerminator"
                                      },GenHS,"getTerminator")
                          ]
             }
       ,Spec { specHeader = irInclude version "InlineAsm.h"
             , specNS = llvmNS
             , specName = "InlineAsm"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Metadata.h"
             , specNS = llvmNS
             , specName = "MDNode"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Metadata.h"
             , specNS = llvmNS
             , specName = "MDString"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = "llvm/CodeGen/PseudoSourceValue.h"
             , specNS = llvmNS
             , specName = "FixedStackPseudoSourceValue"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Constant.h"
             , specNS = llvmNS
             , specName = "Constant"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          (if version>=llvm3_1
                           then [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                            , ftName = "getAggregateElement"
                                            , ftArgs = [(False,normalT unsigned)]
                                            , ftOverloaded = True
                                            },GenHS,"constantGetAggregateElement_")]
                           else [])++
                          (if version >= llvm3_3
                           then [(memberFun { ftReturnType = normalT bool
                                            , ftName = "isNullValue"
                                            },GenHS,"isNullValue")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "canTrap"
                                            },GenHS,"canTrap")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isThreadDependent"
                                            },GenHS,"isThreadDependent")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isConstantUsed"
                                            },GenHS,"isConstantUsed")
                                ]
                           else [])
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "BlockAddress"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantArray"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "ArrayType" []
                                      , ftName = "getType"
                                      },GenHS,"constantArrayGetType")]
             }
       ]++
    (if version>=llvm3_1
     then [Spec { specHeader = irInclude version "Constants.h"
                , specNS = llvmNS
                , specName = "ConstantDataSequential"
                , specTemplateArgs = []
                , specType = ClassSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "SequentialType" []
                                         , ftName = "getType"
                                         , ftOverloaded = True
                                         },GenHS,"constantDataSequentialGetType")
                             ,(memberFun { ftReturnType = normalT unsigned
                                         , ftName = "getNumElements"
                                         , ftOverloaded = True
                                         },GenHS,"constantDataSequentialGetNumElements_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                         , ftName = "getElementAsConstant"
                                         , ftOverloaded = True
                                         , ftArgs = [(False,normalT unsigned)]
                                         },GenHS,"constantDataSequentialGetElementAsConstant_")]
                }
          ,Spec { specHeader = irInclude version "Constants.h"
                , specNS = llvmNS
                , specName = "ConstantDataArray"
                , specTemplateArgs = []
                , specType = ClassSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "ArrayType" []
                                         , ftName = "getType"
                                         },GenHS,"constantDataArrayGetType")]
                }
          ,Spec { specHeader = irInclude version "Constants.h"
                , specNS = llvmNS
                , specName = "ConstantDataVector"
                , specTemplateArgs = []
                , specType = ClassSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "VectorType" []
                                         , ftName = "getType"
                                         },GenHS,"constantDataVectorGetType")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantExpr"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getOpcode"
                                      },GenHS,"constantExprGetOpcode_")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantFP"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = constT $ ref $ llvmType "APFloat"
                                      , ftName = "getValueAPF"
                                      },GenHS,"constantFPGetValueAPF")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantInt"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "IntegerType" []
                                      , ftName = "getType"
                                      },GenHS,"constantIntGetType")
                          ,(memberFun { ftReturnType = constT $ ref $ llvmType "APInt"
                                      , ftName = "getValue"
                                      },GenHS,"constantIntGetValue")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantPointerNull"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "PointerType" []
                                      , ftName = "getType"
                                      },GenHS,"constantPointerNullGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantStruct"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "StructType" []
                                      , ftName = "getType"
                                      },GenHS,"constantStructGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantVector"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"constantVectorGetType")]
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = llvmNS
             , specName = "GlobalValue"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "PointerType" []
                                      , ftName = "getType"
                                      , ftOverloaded = True
                                      },GenHS,"globalValueGetType")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isDeclaration"
                                      , ftOverloaded = True
                                      },GenHS,"globalValueIsDeclaration_")]
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = llvmNS
             , specName = "GlobalAlias"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = llvmNS
             , specName = "GlobalVariable"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isConstant"
                                      },GenHS,"globalVariableIsConstant")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isThreadLocal"
                                      },GenHS,"globalVariableIsThreadLocal")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                      , ftName = "getInitializer"
                                      },GenHS,"globalVariableGetInitializer")
                          ]
             }
       ,Spec { specHeader = irInclude version "Function.h"
             , specNS = llvmNS
             , specName = "Function"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVarArg"
                                      },GenHS,"functionIsVarArg")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType llvmNS "iplist" 
                                                       [normalT $ NamedType llvmNS "BasicBlock" []]
                                      , ftName = "getBasicBlockList"
                                      },GenHS,"getBasicBlockList")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType llvmNS "BasicBlock" []
                                      , ftName = "getEntryBlock"
                                      },GenHS,"getEntryBlock")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionType"
                                      , ftName = "getFunctionType"
                                      },GenHS,"functionGetFunctionType")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType llvmNS "iplist"
                                                       [normalT $ llvmType "Argument"]
                                      , ftName = "getArgumentList"
                                      },GenHS,"functionGetArgumentList")
                          ]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "UndefValue"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/Support/system_error.h"
                , specNS = llvmNS
                , specName = "error_code"
                , specTemplateArgs = []
                , specType = ClassSpec
                             [(Destructor False,GenHS,"deleteErrorCode")
                             ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                         , ftName = "value"
                                         },GenHS,"errorCodeValue_")]
                }]
     else [])++
       [Spec { specHeader = "llvm/Support/MemoryBuffer.h"
             , specNS = llvmNS
             , specName = "MemoryBuffer"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor False,GenHS,"deleteMemoryBuffer")
                          ,(memberFun { ftReturnType = normalT size_t 
                                      , ftName = "getBufferSize"
                                      },GenHS,"getBufferSize_")
                          ,if version>=llvm2_9
                           then (memberFun { ftReturnType = normalT (NamedType llvmNS "error_code" []) 
                                           , ftName = "getFile"
                                           , ftArgs = [(False,normalT (NamedType llvmNS "StringRef" []))
                                                      ,(False,normalT (RefType $ NamedType llvmNS "OwningPtr" 
                                                                                   [normalT (NamedType llvmNS "MemoryBuffer" [])]))
                                                      ,(False,normalT (NamedType [] "int64_t" []))]++
                                                      (if version>=llvm3_0
                                                       then [(False,normalT (NamedType [] "bool" []))]
                                                       else [])
                                           , ftStatic = True
                                           },GenHS,"getFileMemoryBuffer")
                           else (memberFun { ftReturnType = normalT $ ptr $ llvmType "MemoryBuffer"
                                           , ftName = "getFile"
                                           , ftArgs = [(False,normalT (NamedType llvmNS "StringRef" []))]
                                           , ftStatic = True
                                           },GenHS,"getFileMemoryBuffer")
                          ]
             }
       ,Spec { specHeader = "llvm/Support/SourceMgr.h"
             , specNS = llvmNS
             , specName = "SMDiagnostic"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [],GenHS,"newSMDiagnostic")
                          ,(Destructor False,GenHS,"deleteSMDiagnostic")
                          ,(memberFun { ftReturnType = normalT (NamedType llvmNS "StringRef" [])
                                      , ftName = "getFilename"
                                      },GenHS,"getFilename")
                          ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                      , ftName = "getLineNo"
                                      },GenHS,"getLineNo_")
                          ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                      , ftName = "getColumnNo"
                                      },GenHS,"getColumnNo_")
                          ]
             }
       ,Spec { specHeader = irInclude version "LLVMContext.h"
             , specNS = llvmNS
             , specName = "LLVMContext"
             , specTemplateArgs = []
             , specType = ClassSpec [(Constructor [],GenHS,"newLLVMContext")
                                    ,(Destructor False,GenHS,"deleteLLVMContext")]
             }
       ,Spec { specHeader = irInclude version "LLVMContext.h"
             , specNS = llvmNS
             , specName = "getGlobalContext"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ref $ llvmType "LLVMContext"
                                        , gfunArgs = []
                                        , gfunHSName = "getGlobalContext"
                                        }
             }
       ,Spec { specHeader = irInclude version "Module.h"
             , specNS = llvmNS
             , specName = "Module"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [(False,normalT $ NamedType llvmNS "StringRef" [])
                                        ,(False,normalT $ RefType $ NamedType llvmNS "LLVMContext" [])
                                        ],GenHS,"newModule")
                          ,(Destructor False,GenHS,"deleteModule")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "dump"
                                      },GenHS,"moduleDump")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType llvmNS "iplist"
                                                       [normalT $ NamedType llvmNS "Function" []]
                                      , ftName = "getFunctionList"
                                      },GenHS,"getFunctionList")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType llvmNS "iplist"
                                                       [normalT $ NamedType llvmNS "GlobalVariable" []]
                                      , ftName = "getGlobalList"
                                      },GenHS,"getGlobalList")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "getTypeByName"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      },GenHS,"getTypeByName")
                          ]
             }
       ,Spec { specHeader = "llvm/Support/IRReader.h"
             , specNS = []
             , specName = "llvm"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT (PtrType $ NamedType llvmNS "Module" []) 
                                      , ftName = "ParseIR"
                                      , ftArgs = [(False,normalT (PtrType $ NamedType llvmNS "MemoryBuffer" []))
                                                 ,(False,normalT (RefType $ NamedType llvmNS "SMDiagnostic" []))
                                                 ,(False,normalT (RefType $ NamedType llvmNS "LLVMContext" []))]
                                      , ftStatic = True
                                      },GenHS,"parseIR")
                          {-,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionPass"
                                      , ftName = "createCFGSimplificationPass"
                                      , ftStatic = True
                                      },GenHS,"createCFGSimplificationPass")-}
                          ]++
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isa"
                                      , ftTemplArgs = [to_tp,from_tp]
                                      , ftArgs = [(True,toConstRef from_tp)]
                                      , ftStatic = True
                                      , ftOverloaded = True
                                      , ftPure = True
                                      },GenHS,"isA"++to)
                           | (to,from) <- [(to',"Value")
                                              | to' <- ["Value"
                                                      ,"Argument"
                                                      ,"BasicBlock"
                                                      ,"InlineAsm"
                                                      ,"MDNode"
                                                      ,"MDString"
                                                      ,"PseudoSourceValue"
                                                      ,"User"
                                                      ,"FixedStackPseudoSourceValue"
                                                      ,"Constant"
                                                      ,"BlockAddress"
                                                      ,"ConstantAggregateZero"
                                                      ,"ConstantArray"]++
                                                      (if version>=llvm3_1
                                                       then ["ConstantDataSequential"
                                                            ,"ConstantDataArray"
                                                            ,"ConstantDataVector"]
                                                       else [])++
                                                      ["ConstantExpr"
                                                      {-,"BinaryConstantExpr"
                                                        ,"CompareConstantExpr"
                                                        ,"ExtractElementConstantExpr"
                                                        ,"ExtractValueConstantExpr"
                                                        ,"GetElementPtrConstantExpr"
                                                        ,"InsertElementConstantExpr"
                                                        ,"InsertValueConstantExpr"
                                                        ,"SelectConstantExpr"
                                                        ,"ShuffleVectorConstantExpr"
                                                        ,"UnaryConstantExpr"-}
                                                      ,"ConstantFP"
                                                      ,"ConstantInt"
                                                      ,"ConstantPointerNull"
                                                      ,"ConstantStruct"
                                                      ,"ConstantVector"
                                                      ,"GlobalValue"
                                                      ,"Function"
                                                      ,"GlobalAlias"
                                                      ,"GlobalVariable"
                                                      ,"UndefValue"
                                                      ,"Instruction"]++
                                                      (if version>=llvm3_0
                                                       then ["AtomicCmpXchgInst"
                                                            ,"AtomicRMWInst"]
                                                       else [])++
                                                      ["BinaryOperator"
                                                      ,"CallInst"
                                                      ,"CmpInst"
                                                      ,"FCmpInst"
                                                      ,"ICmpInst"
                                                      ,"ExtractElementInst"]++
                                                      (if version>=llvm3_0
                                                       then ["FenceInst"]
                                                       else [])++
                                                      ["GetElementPtrInst"
                                                      ,"InsertElementInst"
                                                      ,"InsertValueInst"]++
                                                      (if version>=llvm3_0
                                                       then ["LandingPadInst"]
                                                       else [])++
                                                      ["PHINode"
                                                      ,"SelectInst"
                                                      ,"ShuffleVectorInst"
                                                      ,"StoreInst"
                                                      ,"TerminatorInst"
                                                      ,"BranchInst"
                                                      ,"IndirectBrInst"
                                                      ,"InvokeInst"]++
                                                      (if version>=llvm3_0
                                                       then ["ResumeInst"]
                                                       else [])++
                                                      ["ReturnInst"
                                                      ,"SwitchInst"
                                                      ,"UnreachableInst"
                                                      ,"UnaryInstruction"
                                                      ,"AllocaInst"
                                                      ,"CastInst"
                                                      ,"BitCastInst"
                                                      ,"FPExtInst"
                                                      ,"FPToSIInst"
                                                      ,"FPToUIInst"
                                                      ,"FPTruncInst"
                                                      ,"IntToPtrInst"
                                                      ,"PtrToIntInst"
                                                      ,"SExtInst"
                                                      ,"SIToFPInst"
                                                      ,"TruncInst"
                                                      ,"UIToFPInst"
                                                      ,"ZExtInst"
                                                      ,"ExtractValueInst"
                                                      ,"LoadInst"
                                                      ,"VAArgInst"
                                                      ,"Operator"
                                                      ]]++
                                         [(to',"Type") 
                                              | to' <- ["Type"
                                                      ,"CompositeType"
                                                      ,"SequentialType"
                                                      ,"ArrayType"
                                                      ,"PointerType"
                                                      ,"VectorType"
                                                      ,"StructType"
                                                      ,"FunctionType"
                                                      ,"IntegerType"]]
                          , let to_tp = normalT $ NamedType llvmNS to []
                                from_tp = normalT $ NamedType llvmNS from []
                          ]
             }
       ,Spec { specHeader = "llvm/CodeGen/PseudoSourceValue.h"
             , specNS = llvmNS
             , specName = "PseudoSourceValue"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = llvmNS
             , specName = "ConstantAggregateZero"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instruction.h"
             , specNS = llvmNS
             , specName = "Instruction"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "BasicBlock" []
                                      , ftName = "getParent"
                                      },GenHS,"instructionGetParent")
                          ,(memberFun { ftReturnType = constT $ ref $ NamedType llvmNS "DebugLoc" []
                                      , ftName = "getDebugLoc"
                                      },GenHS,"instructionGetDebugLoc")]
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "AtomicCmpXchgInst"
                , specTemplateArgs = []
                , specType = ClassSpec 
                             [(memberFun { ftReturnType = normalT bool
                                         , ftName = "isVolatile"
                                         },GenHS,"atomicCmpXchgInstIsVolatile")
                             ,(memberFun { ftReturnType = normalT int
                                         , ftName = "getOrdering"
                                         },GenHS,"atomicCmpXchgInstGetOrdering_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getPointerOperand"
                                         },GenHS,"atomicCmpXchgInstGetPointerOperand")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getCompareOperand"
                                         },GenHS,"atomicCmpXchgInstGetCompareOperand")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getNewValOperand"
                                         },GenHS,"atomicCmpXchgInstGetNewValOperand")
                             ,(Constructor [(True,normalT $ ptr $ llvmType "Value")
                                           ,(True,normalT $ ptr $ llvmType "Value")
                                           ,(True,normalT $ ptr $ llvmType "Value")
                                           ,(False,normalT $ EnumType llvmNS "AtomicOrdering")
                                           ,(False,normalT $ EnumType llvmNS "SynchronizationScope")
                                           ],GenHS,"newAtomicCmpXchgInst_")]
                }
          ,Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "AtomicRMWInst"
                , specTemplateArgs = []
                , specType = ClassSpec 
                             [(memberFun { ftReturnType = normalT int
                                         , ftName = "getOperation"
                                         },GenHS,"atomicRMWInstGetOperation_")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isVolatile"
                                         },GenHS,"atomicRMWInstIsVolatile")
                             ,(memberFun { ftReturnType = normalT int
                                         , ftName = "getOrdering"
                                         },GenHS,"atomicRMWInstGetOrdering_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getPointerOperand"
                                         },GenHS,"atomicRMWInstGetPointerOperand")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getValOperand"
                                         },GenHS,"atomicRMWInstGetValOperand")
                             ,(Constructor [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "AtomicRMWInst" []] "BinOp")
                                           ,(True,normalT $ ptr $ llvmType "Value")
                                           ,(True,normalT $ ptr $ llvmType "Value")
                                           ,(False,normalT $ EnumType llvmNS "AtomicOrdering")
                                           ,(False,normalT $ EnumType llvmNS "SynchronizationScope")
                                           ],GenHS,"newAtomicRMWInst_")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "BinaryOperator"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getOpcode"
                                      },GenHS,"binOpGetOpCode_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BinaryOperator"
                                      , ftName = "Create"
                                      , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "Instruction" []] "BinaryOps")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newBinaryOperator_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "CallInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isTailCall"
                                      },GenHS,"callInstIsTailCall")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumArgOperands"
                                      },GenHS,"callInstGetNumArgOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Value" []
                                      , ftName = "getArgOperand"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"callInstGetArgOperand_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getCallingConv"
                                      },GenHS,"callInstGetCallingConv_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Value" []
                                      , ftName = "getCalledValue"
                                      },GenHS,"callInstGetCalledValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "CallInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,if version>=llvm3_0
                                                  then (False,normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Value"])
                                                  else (False,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newCallInst_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "CmpInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getPredicate" 
                                      , ftOverloaded = True
                                      },GenHS,"cmpInstGetPredicate_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FCmpInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor
                            [(False,normalT $ EnumType [ClassName "llvm" []
                                                       ,ClassName "CmpInst" []] "Predicate")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(False,constT $ ref $ llvmType "Twine")
                            ],GenHS,"newFCmpInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ICmpInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor
                            [(False,normalT $ EnumType [ClassName "llvm" []
                                                       ,ClassName "CmpInst" []] "Predicate")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(True,normalT $ ptr $ llvmType "Value")
                            ,(False,constT $ ref $ llvmType "Twine")
                            ],GenHS,"newICmpInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ExtractElementInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Value" []
                                      , ftName = "getVectorOperand"
                                      },GenHS,"extractElementInstGetVectorOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Value" []
                                      , ftName = "getIndexOperand"
                                      },GenHS,"extractElementInstGetIndexOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ExtractElementInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newExtractElementInst_")]
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "FenceInst"
                , specTemplateArgs = []
                , specType = ClassSpec 
                             [(memberFun { ftReturnType = normalT int
                                         , ftName = "getOrdering"
                                         },GenHS,"fenceInstGetOrdering_")
                             ,(Constructor
                               [(False,normalT $ ref $ llvmType "LLVMContext")
                               ,(False,normalT $ EnumType llvmNS "AtomicOrdering")
                               ,(False,normalT $ EnumType llvmNS "SynchronizationScope")
                               ],GenHS,"newFenceInst_")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "GetElementPtrInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "PointerType" []
                                      , ftName = "getType"
                                      },GenHS,"getElementPtrInstGetType")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isInBounds"
                                      },GenHS,"getElementPtrInstIsInBounds")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getPointerOperand"
                                      },GenHS,"getElementPtrInstGetPointerOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                      , ftName = "idx_begin"
                                      },GenHS,"getElementPtrInstIdxBegin")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                      , ftName = "idx_end"
                                      },GenHS,"getElementPtrInstIdxEnd")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumIndices"
                                      },GenHS,"getElementPtrInstGetNumIndices_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "GetElementPtrInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,if version>=llvm3_0
                                                  then (False,normalT $ NamedType llvmNS "ArrayRef" [normalT $ ptr $ llvmType "Value"])
                                                  else (False,normalT $ ptr $ llvmType "Value")
                                                 ,(False,normalT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newGetElementPtrInst_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "InsertElementInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"insertElementInstGetType")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "InsertElementInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newInsertElementInst_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "InsertValueInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "InsertValueInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,if version>=llvm3_0
                                                  then (False,normalT $ NamedType llvmNS "ArrayRef" [normalT unsigned])
                                                  else (False,normalT unsigned)
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newInsertValueInst_")]
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "LandingPadInst"
                , specTemplateArgs = []
                , specType = ClassSpec
                             [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getPersonalityFn"
                                         },GenHS,"landingPadInstGetPersonaliteFn")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isCleanup"
                                         },GenHS,"landingPadInstIsCleanup")
                             ,(memberFun { ftReturnType = normalT unsigned
                                         , ftName = "getNumClauses"
                                         },GenHS,"landingPadInstGetNumClauses_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                         , ftName = "getClause"
                                         , ftArgs = [(False,normalT unsigned)]
                                         },GenHS,"landingPadInstGetClause_")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isCatch"
                                         , ftArgs = [(False,normalT unsigned)]
                                         },GenHS,"landingPadInstIsCatch_")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "isFilter"
                                         , ftArgs = [(False,normalT unsigned)]
                                         },GenHS,"landingPadInstIsFilter_")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "LandingPadInst"
                                         , ftName = "Create"
                                         , ftArgs = [(True,normalT $ ptr $ llvmType "Type")
                                                    ,(True,normalT $ ptr $ llvmType "Value")
                                                    ,(False,normalT unsigned)
                                                    ,(False,constT $ ref $ llvmType "Twine")]
                                         , ftStatic = True
                                         },GenHS,"newLandingPadInst_")
                             ,(memberFun { ftName = "setCleanup"
                                         , ftArgs = [(False,normalT bool)]
                                         },GenHS,"landingPadInstSetCleanup")
                             ,(memberFun { ftName = "addClause"
                                         , ftArgs = [(True,normalT $ ptr $ llvmType "Value")]
                                         },GenHS,"landingPadInstAddClause_")
                             ]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "PHINode"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumIncomingValues"
                                      },GenHS,"phiNodeGetNumIncomingValues_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Value" []
                                      , ftName = "getIncomingValue"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"phiNodeGetIncomingValue_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "BasicBlock" []
                                      , ftName = "getIncomingBlock"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"phiNodeGetIncomingBlock_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "PHINode"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]++
                                                 (if version>=llvm3_0
                                                  then [(False,normalT unsigned)]
                                                  else [])++
                                                 [(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newPhiNode_")
                          ,(memberFun { ftName = "addIncoming"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,normalT $ ptr $ llvmType "BasicBlock")]
                                      },GenHS,"phiNodeAddIncoming_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SelectInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCondition"
                                      },GenHS,"selectInstGetCondition")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getTrueValue"
                                      },GenHS,"selectInstGetTrueValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getFalseValue"
                                      },GenHS,"selectInstGetFalseValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "SelectInst"
                                      , ftName = "Create"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(True,normalT $ ptr $ llvmType "Value")
                                                 ,(False,constT $ ref $ llvmType "Twine")]
                                      , ftStatic = True
                                      },GenHS,"newSelectInst_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ShuffleVectorInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"shuffleVectorInstGetType")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "StoreInst"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },GenHS,"storeInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },GenHS,"storeInstGetAlignment_")]++
               (if version>=llvm3_0
                then [(memberFun { ftReturnType = normalT int
                                 , ftName = "getOrdering"
                                 },GenHS,"storeInstGetOrdering_")]
                else [])++
               [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                           , ftName = "getValueOperand"
                           },GenHS,"storeInstGetValueOperand")
               ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                           , ftName = "getPointerOperand"
                           },GenHS,"storeInstGetPointerOperand")
               ]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "TerminatorInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumSuccessors"
                                      , ftOverloaded = True
                                      },GenHS,"terminatorInstGetNumSuccessors_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getSuccessor"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },GenHS,"terminatorInstGetSuccessor_")]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "BranchInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isConditional"
                                      },GenHS,"branchInstIsConditional")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCondition"
                                      },GenHS,"branchInstGetCondition")]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "IndirectBrInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "InvokeInst"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumArgOperands"
                                      },GenHS,"invokeInstGetNumArgOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftName = "getArgOperand"
                                      },GenHS,"invokeInstGetArgOperand_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getCallingConv"
                                      },GenHS,"invokeInstGetCallingConv_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCalledValue"
                                      },GenHS,"invokeInstGetCalledValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getNormalDest"
                                      },GenHS,"invokeInstGetNormalDest")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getUnwindDest"
                                      },GenHS,"invokeInstGetUnwindDest")]++
                          (if version>=llvm3_0
                           then [(memberFun { ftReturnType = normalT $ ptr $ llvmType "LandingPadInst"
                                            , ftName = "getLandingPadInst"
                                            },GenHS,"invokeInstGetLandingPadInst")]
                           else [])
             }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "InstrTypes.h"
                , specNS = llvmNS
                , specName = "ResumeInst"
                , specTemplateArgs = []
                , specType = ClassSpec []
                }]
     else [])++
       [Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "ReturnInst"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getReturnValue"
                                      },GenHS,"returnInstGetReturnValue")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SwitchInst"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCondition"
                                      },GenHS,"switchInstGetCondition")]++
                          (if version>=llvm3_1
                           then [(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" []
                                            , ftName = "case_begin"
                                            },GenHS,"switchInstCaseBegin")
                                ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" []
                                            , ftName = "case_end"
                                            },GenHS,"switchInstCaseEnd")
                                ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" []
                                            , ftName = "case_default"
                                            },GenHS,"switchInstCaseDefault")]
                           else [])
             }]++
    (if version>=llvm3_1
     then [Spec { specHeader = irInclude version "InstrTypes.h"
                , specNS = [ClassName "llvm" [],ClassName "SwitchInst" []]
                , specName = "CaseIt"
                , specTemplateArgs = []
                , specType = ClassSpec
                             [(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" []
                                         , ftName = "operator++"
                                         },GenHS,"caseItNext")
                             ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" []
                                         , ftName = "operator--"
                                         },GenHS,"caseItPrev")
                             ,(memberFun { ftReturnType = normalT bool
                                         , ftName = "operator=="
                                         , ftArgs = [(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "SwitchInst" []] "CaseIt" [])]
                                         },GenHS,"caseItEq")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ConstantInt"
                                         , ftName = "getCaseValue"
                                         },GenHS,"caseItGetCaseValue")
                             ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                         , ftName = "getCaseSuccessor"
                                         },GenHS,"caseItGetCaseSuccessor")]
                }]
     else [])++
       [Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "UnreachableInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "UnaryInstruction"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "AllocaInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "PointerType" []
                                      , ftName = "getType"
                                      },GenHS,"allocaInstGetType")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "isArrayAllocation"
                                      },GenHS,"allocaInstIsArrayAllocation")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getArraySize"
                                      },GenHS,"allocaInstGetArraySize")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },GenHS,"allocaInstGetAlignment_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "CastInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "BitCastInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FPExtInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FPToUIInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FPTruncInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "IntToPtrInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "PtrToIntInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SExtInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "SIToFPInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "TruncInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "UIToFPInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ZExtInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ExtractValueInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "LoadInst"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },GenHS,"loadInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },GenHS,"loadInstGetAlignment_")]++
               (if version>=llvm3_0
                then [(memberFun { ftReturnType = normalT int
                                 , ftName = "getOrdering"
                                 },GenHS,"loadInstGetOrdering_")]
                else [])++
               [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                           , ftName = "getPointerOperand"
                           },GenHS,"loadInstGetPointerOperand")
               ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "VAArgInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "User.h"
             , specNS = llvmNS
             , specName = "User"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumOperands" 
                                      , ftOverloaded = True
                                      },GenHS,"getNumOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Value" []
                                      , ftName = "getOperand" 
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },GenHS,"getOperand_")]
             }
       ,Spec { specHeader = irInclude version "Operator.h"
             , specNS = llvmNS
             , specName = "Operator"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Use.h"
             , specNS = llvmNS
             , specName = "Use"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "get"
                                      },GenHS,"useGet")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Use"
                                      , ftName = "getNext"
                                      },GenHS,"useGetNext")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "User"
                                      , ftName = "getUser"
                                      },GenHS,"useGetUser")]
               
             }
       ,Spec { specHeader = "llvm/PassManager.h"
             , specNS = llvmNS
             , specName = "PassManager"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [],GenHS,"newPassManager")
                          ,(Destructor False,GenHS,"deletePassManager")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "add"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Pass")]
                                      },GenHS,"passManagerAdd_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "run"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Module")]
                                      },GenHS,"passManagerRun")
                          ]
             }
       ,Spec { specHeader = "llvm/PassManager.h"
             , specNS = llvmNS
             , specName = "FunctionPassManager"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [(False,normalT $ ptr $ llvmType "Module")],GenHS,"newFunctionPassManager")
                          ,(Destructor False,GenHS,"deleteFunctionPassManager")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "add"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Pass")]
                                      },GenHS,"functionPassManagerAdd_")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "run"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Function")]
                                      },GenHS,"functionPassManagerRun")
                          ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "Pass"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(Destructor True,GenHS,"deletePass_")
                          ,(memberFun { ftReturnType = constT $ ptr $ llvmType "PassInfo"
                                      , ftName = "lookupPassInfo"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      , ftStatic = True
                                      },GenHS,"passLookupPassInfo")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "AnalysisResolver"
                                      , ftName = "getResolver"
                                      , ftOverloaded = True
                                      },GenHS,"passGetResolver_")
                          ,(memberFun { ftReturnType = normalT $ ptr void
                                      , ftName = "getAdjustedAnalysisPointer"
                                      , ftArgs = [(False,constT $ ptr void)]
                                      , ftOverloaded = True
                                      },GenHS,"passGetAdjustedAnalysisPointer_")
                          ,(memberFun { ftReturnType = normalT $ EnumType llvmNS "PassKind"
                                      , ftName = "getPassKind"
                                      , ftOverloaded = True
                                      },GenHS,"passGetKind_")
                          ,(memberFun { ftReturnType = constT $ ptr char
                                      , ftName = "getPassName"
                                      , ftOverloaded = True
                                      },GenHS,"passGetName_")
                          ,(memberFun { ftName = "dump"
                                      , ftOverloaded = True
                                      },GenHS,"passDump_")
                          ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "FunctionPass"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "runOnFunction"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Function")]
                                      , ftOverloaded = True
                                      },GenHS,"functionPassRun_")]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "ModulePass"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "runOnModule"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "Module")]
                                      , ftOverloaded = True
                                      },GenHS,"modulePassRunOnModule_")
                          ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "ImmutablePass"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = "llvm/Analysis/FindUsedTypes.h"
             , specNS = llvmNS
             , specName = "FindUsedTypes"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(Constructor [],GenHS,"newFindUsedTypes")
                          ,(Destructor False,GenHS,"deleteFindUsedTypes")
                          ,(memberFun { ftReturnType = constT $ ref $ NamedType [ClassName "llvm" []] "SetVector" [normalT $ ptr $ llvmType "Type"]
                                      , ftName = "getTypes"
                                      },GenHS,"findUsedTypesGetTypes")]
             }]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/Target/TargetLibraryInfo.h"
                , specNS = llvmNS
                , specName = "TargetLibraryInfo"
                , specTemplateArgs = []
                , specType = ClassSpec $
                             [(Constructor [],GenHS,"newTargetLibraryInfo")
                             ,(Destructor False,GenHS,"deleteTargetLibraryInfo")]++
                             (if version>=llvm3_3
                              then [(memberFun { ftReturnType = normalT bool
                                               , ftName = "getLibFunc"
                                               , ftArgs = [(False,normalT $ llvmType "StringRef")
                                                          ,(False,normalT $ ref $ EnumType [ClassName "llvm" [],ClassName "LibFunc" []] "Func")]
                                               },GenHS,"targetLibraryInfoGetLibFunc_")]
                              else [])++
                             (if version>=llvm3_1
                              then [(memberFun { ftReturnType = normalT $ llvmType "StringRef"
                                               , ftName = "getName"
                                               , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "LibFunc" []] "Func")]
                                               },GenHS,"targetLibraryInfoGetName_")]
                              else [])++
                             [(memberFun { ftReturnType = normalT bool
                                         , ftName = "has"
                                         , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" [],ClassName "LibFunc" []] "Func")]
                                         },GenHS,"targetLibraryInfoHas_")
                             ]
                }]
     else [])++
       (if version >= llvm3_2
        then [Spec { specHeader = irInclude version "DataLayout.h"
                   , specNS = llvmNS
                   , specName = "DataLayout"
                   , specTemplateArgs = []
                   , specType = ClassSpec
                                [(Constructor [(False,normalT $ llvmType "StringRef")],GenHS,"newDataLayoutFromString")
                                ,(Constructor [(False,constT $ ptr $ llvmType "Module")],GenHS,"newDataLayoutFromModule")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isLittleEndian"
                                            },GenHS,"dataLayoutIsLittleEndian")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isBigEndian"
                                            },GenHS,"dataLayoutIsBigEndian")]
                   }]
        else [Spec { specHeader = "llvm/Target/TargetData.h"
                   , specNS = llvmNS
                   , specName = "TargetData"
                   , specTemplateArgs = []
                   , specType = ClassSpec
                                [(Constructor [(False,normalT $ llvmType "StringRef")],GenHS,"newTargetDataFromString")
                                ,(Constructor [(False,constT $ ptr $ llvmType "Module")],GenHS,"newTargetDataFromModule")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isLittleEndian"
                                            },GenHS,"targetDataIsLittleEndian")
                                ,(memberFun { ftReturnType = normalT bool
                                            , ftName = "isBigEndian"
                                            },GenHS,"targetDataIsBigEndian")]
                   }])++
       [Spec { specHeader = "llvm/PassSupport.h"
             , specNS = llvmNS
             , specName = "PassInfo"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor False,GenHS,"deletePassInfo")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Pass"
                                      , ftName = "createPass"
                                      },GenHS,"passInfoCreatePass")
                          ,(memberFun { ftReturnType = constT $ ptr $ char
                                      , ftName = "getPassName"
                                      },GenHS,"passInfoGetPassName_")
                          ,(memberFun { ftReturnType = constT $ ptr $ char
                                      , ftName = "getPassArgument"
                                      },GenHS,"passInfoGetPassArgument_")
                          ]
             }
       ,Spec { specHeader = "llvm/Analysis/LoopInfo.h"
             , specNS = llvmNS
             , specName = "LoopInfo"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [],GenHS,"newLoopInfo")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType llvmNS "LoopInfoBase" 
                                                       [normalT $ llvmType "BasicBlock"
                                                       ,normalT $ llvmType "Loop"]
                                      , ftName = "getBase"
                                      },GenHS,"loopInfoGetBase")
                          ]
             }
       ]++
    (if version>=llvm3_0
     then [Spec { specHeader = "llvm/Transforms/IPO/PassManagerBuilder.h"
                , specNS = llvmNS
                , specName = "PassManagerBuilder"
                , specTemplateArgs = []
                , specType = ClassSpec $
                             [(Constructor [],GenHS,"newPassManagerBuilder")
                             ,(Destructor False,GenHS,"deletePassManagerBuilder")
                             ,(memberFun { ftName = "populateFunctionPassManager"
                                         , ftArgs = [(False,normalT $ ref $ llvmType "FunctionPassManager")
                                                    ]
                                         },GenHS,"populateFunctionPassManager")
                             ,(Setter { ftSetVar = "OptLevel"
                                      , ftSetType = normalT unsigned
                                      },GenHS,"setPassManagerBuilderOptLevel")
                             ,(Setter { ftSetVar = "SizeLevel"
                                      , ftSetType = normalT unsigned
                                      },GenHS,"setPassManagerBuilderSizeLevel")
                             ,(Setter { ftSetVar = "Inliner"
                                      , ftSetType = normalT $ ptr $ llvmType "Pass"
                                      },GenHS,"setPassManagerBuilderInliner")
                             ,(Setter { ftSetVar = "DisableSimplifyLibCalls"
                                      , ftSetType = normalT bool
                                      },GenHS,"setPassManagerBuilderDisableSimplifyLibCalls")
                             ,(Setter { ftSetVar = "DisableUnitAtATime"
                                      , ftSetType = normalT bool
                                      },GenHS,"setPassManagerBuilderDisableUnitAtATime")
                             ,(Setter { ftSetVar = "DisableUnrollLoops"
                                      , ftSetType = normalT bool
                                      },GenHS,"setPassManagerBuilderDisableUnrollLoops")]++
                             (if version>=llvm3_1
                              then [(Setter { ftSetVar = "Vectorize"
                                            , ftSetType = normalT bool
                                            },GenHS,"setPassManagerBuilderVectorize")]
                              else [])
                }]
     else [])++
       [Spec { specHeader = "llvm/Transforms/Scalar.h"
             , specNS = llvmNS
             , specName = f
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "Pass"
                                        , gfunArgs = []
                                        , gfunHSName = f
                                        }
             } | f <- ["createCFGSimplificationPass"
                     ,"createConstantPropagationPass"
                     ,"createDemoteRegisterToMemoryPass"
                     ,"createGVNPass"
                     ,"createInstructionCombiningPass"
                     ,"createPromoteMemoryToRegisterPass"
                     ,"createReassociatePass"
                     ,"createAggressiveDCEPass"
                     ,"createDeadStoreEliminationPass"
                     ,"createIndVarSimplifyPass"
                     ,"createJumpThreadingPass"
                     ,"createLICMPass"
                     ,"createLoopDeletionPass"
                     ,"createLoopRotatePass"
                     ,"createLoopSimplifyPass"
                     ,"createLoopStrengthReducePass"
                     ,"createLoopUnrollPass"
                     ,"createLoopUnswitchPass"
                     ,"createMemCpyOptPass"
                     ,"createSCCPPass"
                     ,"createScalarReplAggregatesPass"
                     ,"createSimplifyLibCallsPass"
                     ,"createTailCallEliminationPass"
                     ]
       ]++
       [Spec { specHeader = "llvm/Transforms/IPO.h"
             , specNS = llvmNS
             , specName = f
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType p
                                        , gfunArgs = fmap (\x -> (False,x)) a
                                        , gfunHSName = f
                                        }
             } | (p,f,a) <- [("ModulePass","createStripSymbolsPass",[normalT bool])
                           ,("ModulePass","createStripNonDebugSymbolsPass",[])
                           ,("ModulePass","createStripDebugDeclarePass",[])
                           ,("ModulePass","createStripDeadDebugInfoPass",[])
                           ,("ModulePass","createConstantMergePass",[])
                           ,("ModulePass","createGlobalOptimizerPass",[])
                           ,("ModulePass","createGlobalDCEPass",[])
                           ,("Pass","createFunctionInliningPass",[normalT int])
                           ,("Pass","createAlwaysInlinerPass",if version>=llvm3_1
                                                              then [normalT bool]
                                                              else [])
                           ,("Pass","createPruneEHPass",[])
                           ,("ModulePass","createInternalizePass",[if version>=llvm3_0
                                                                   then normalT $ NamedType llvmNS "ArrayRef" [constT $ ptr $ char]
                                                                   else constT $ NamedType [ClassName "std" []] "vector" [constT $ ptr char]])
                           ,("ModulePass","createDeadArgEliminationPass",[])
                           ,("ModulePass","createDeadArgHackingPass",[])
                           ,("Pass","createArgumentPromotionPass",[normalT unsigned])
                           ,("ModulePass","createIPConstantPropagationPass",[])
                           ,("ModulePass","createIPSCCPPass",[])
                           ,("Pass","createLoopExtractorPass",[])
                           ,("Pass","createSingleLoopExtractorPass",[])
                           ,("ModulePass","createBlockExtractorPass",[])
                           ,("ModulePass","createStripDeadPrototypesPass",[])
                           ,("Pass","createFunctionAttrsPass",[])
                           ,("ModulePass","createMergeFunctionsPass",[])
                           ,("ModulePass","createPartialInliningPass",[])]++
        (if version>=llvm3_3
         then [("ModulePass","createMetaRenamerPass",[])
              ,("ModulePass","createBarrierNoopPass",[])]
         else [])
       ]++
       [Spec { specHeader = "llvm/Analysis/Verifier.h"
             , specNS = llvmNS
             , specName = "createVerifierPass"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "FunctionPass"
                                        , gfunArgs = []
                                        , gfunHSName = "createVerifierPass"
                                        }
             }
       ,Spec { specHeader = "llvm/Support/raw_ostream.h"
             , specNS = llvmNS
             , specName = "raw_ostream"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor True,GenHS,"deleteOStream_")
                          ]
             }
       ,Spec { specHeader = "llvm/Support/raw_ostream.h"
             , specNS = llvmNS
             , specName = "raw_fd_ostream"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [(False,normalT int),(False,normalT bool),(False,normalT bool)],GenHS,"newFDOStream_")
                          ,(Destructor False,GenHS,"deleteFDOStream")]
             }
       ,Spec { specHeader = "llvm/Analysis/AliasAnalysis.h"
             , specNS = llvmNS
             , specName = "AliasAnalysis"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(Constructor [],GenHS,"newAliasAnalysis")
                          ,(Destructor True,GenHS,"deleteAliasAnalysis_")]++
                          (if version>=llvm3_3
                           then [(memberFun { ftReturnType = constT $ ptr $ llvmType "TargetLibraryInfo"
                                            , ftName = "getTargetLibraryInfo"
                                            , ftOverloaded = True
                                            },GenHS,"aliasAnalysisGetTargetLibraryInfo_")]
                           else [])++
                          [(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getTypeStoreSize"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")]
                                      , ftOverloaded = True
                                      },GenHS,"aliasAnalysisGetTypeStoreSize_")]++
                          (if version>=llvm2_9
                           then [(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" []
                                            , ftName = "getLocation"
                                            , ftArgs = [(False,constT $ ptr $ llvmType (inst++"Inst"))]
                                            , ftOverloaded = True
                                            },GenHS,"aliasAnalysisGetLocation"++inst++"_")
                                 | inst <- ["Load","Store","VAArg"]++(if version>=llvm3_0
                                                                     then ["AtomicCmpXchg","AtomicRMW"]
                                                                     else [])]
                           else [])++
                          [(memberFun { ftReturnType = normalT $ EnumType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "AliasResult"
                                      , ftName = "alias"
                                      , ftArgs = if version>=llvm2_9
                                                 then [(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" [])
                                                      ,(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" [])]
                                                 else [(False,constT $ ptr $ llvmType "Value")
                                                      ,(False,normalT unsigned)
                                                      ,(False,constT $ ptr $ llvmType "Value")
                                                      ,(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },GenHS,"aliasAnalysisAlias_")
                          ]
             }]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/Analysis/AliasAnalysis.h"
                , specNS = [ClassName "llvm" [],ClassName "AliasAnalysis" []]
                , specName = "Location"
                , specTemplateArgs = []
                , specType = ClassSpec
                             [(Constructor [(True,constT $ ptr $ llvmType "Value")
                                           ,(False,normalT uint64_t)
                                           ,(False,constT $ ptr $ llvmType "MDNode")
                                           ],GenHS,"newLocation_")]
                }]
     else [])++
       [Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = "getMallocAllocatedType"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "Type"
                                        , gfunArgs = [(False,constT $ ptr $ llvmType "CallInst")]++
                                                     (if version>=llvm3_2
                                                      then [(False,constT $ ptr $ llvmType "TargetLibraryInfo")]
                                                      else [])
                                        , gfunHSName = "getMallocAllocatedType"
                                        }
             }
       ,Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = "getMallocArraySize"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "Value"
                                        , gfunArgs = [(False,normalT $ ptr $ llvmType "CallInst")]++
                                                     (if version >= llvm3_2
                                                      then [(False,constT $ ptr $ llvmType "DataLayout")
                                                           ,(False,constT $ ptr $ llvmType "TargetLibraryInfo")]
                                                      else [(False,constT $ ptr $ llvmType "TargetData")])++
                                                     [(False,normalT bool)]
                                        , gfunHSName = "getMallocArraySize"
                                        }
             }
       ,Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = if version>=llvm3_2
                          then "isMallocLikeFn"
                          else "isMalloc"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT bool
                                        , gfunArgs = [(True,constT $ ptr $ llvmType "Value")]++
                                                     (if version>=llvm3_2
                                                      then [(False,constT $ ptr $ llvmType "TargetLibraryInfo")
                                                           ,(False,normalT bool)]
                                                      else [])
                                        , gfunHSName = "isMallocLikeFn_"
                                        }
             }
       ,Spec { specHeader = "llvm/ADT/Twine.h"
             , specNS = llvmNS
             , specName = "Twine"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [],GenHS,"newTwineEmpty")
                          ,(Constructor [(False,constT $ ptr char)],GenHS,"newTwineString_")
                          ]
             }
       ,Spec { specHeader = "llvm/Analysis/LoopInfo.h"
             , specNS = llvmNS
             , specName = "Loop"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isLoopInvariant"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Value")]
                                      },GenHS,"loopIsLoopInvariant_")
                          ,(memberFun { ftName = "dump"
                                      },GenHS,"loopDump")]
             }
       ]++
    concat
    [[Spec { specHeader = "llvm/Analysis/LoopInfo.h"
           , specNS = llvmNS
           , specName = "LoopBase"
           , specTemplateArgs = [blk,loop]
           , specType = ClassSpec
                        [(memberFun { ftReturnType = normalT unsigned
                                    , ftName = "getLoopDepth"
                                    , ftOverloaded = True
                                    },GenHS,"loopGetDepth_")
                        ,(memberFun { ftReturnType = toPtr blk
                                    , ftName = "getHeader"
                                    , ftOverloaded = True
                                    },GenHS,"loopGetHeader_")
                        ,(memberFun { ftReturnType = toPtr loop
                                    , ftName = "getParentLoop"
                                    , ftOverloaded = True
                                    },GenHS,"loopGetParent_")
                        ,(memberFun { ftReturnType = normalT bool
                                    , ftName = "contains"
                                    , ftArgs = [(False,toConstPtr loop)]
                                    , ftOverloaded = True
                                    },GenHS,"loopContainsLoop_")
                        ,(memberFun { ftReturnType = normalT bool
                                    , ftName = "contains"
                                    , ftArgs = [(False,toConstPtr blk)]
                                    , ftOverloaded = True
                                    },GenHS,"loopContainsBlock_")
                        ,(memberFun { ftReturnType = constT $ NamedType [ClassName "std" []] "vector" [toPtr loop]
                                    , ftName = "getSubLoops"
                                    , ftOverloaded = True
                                    },GenHS,"loopGetSubLoops_")
                        ,(memberFun { ftReturnType = constT $ NamedType [ClassName "std" []] "vector" [toPtr blk]
                                    , ftName = "getBlocks"
                                    , ftOverloaded = True
                                    },GenHS,"loopGetBlocks_")
                        ,(memberFun { ftName = "getExitEdges"
                                    , ftArgs = [(False,normalT $ ref $ NamedType llvmNS "SmallVector"
                                                          [normalT $ NamedType [ClassName "std" []] "pair" [constT $ ptr $ llvmType "BasicBlock"
                                                                                                           ,constT $ ptr $ llvmType "BasicBlock"]
                                                          ,TypeInt 16])]
                                    , ftOverloaded = True
                                    },GenHS,"loopGetExitEdges_")]
             }
     ,Spec { specHeader = "llvm/Analysis/LoopInfo.h"
           , specNS = llvmNS
           , specName = "LoopInfoBase"
           , specTemplateArgs = [blk,loop]
           , specType = ClassSpec
                        [(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" []
                                                                         ,ClassName "vector" [toPtr loop]
                                                                         ] "const_iterator" []
                                    , ftName = "begin"
                                    },GenHS,"loopInfoBaseBegin_")
                        ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" []
                                                                         ,ClassName "vector" [toPtr loop]
                                                                         ] "const_iterator" []
                                    , ftName = "end"
                                    },GenHS,"loopInfoBaseEnd_")
                        ,(memberFun { ftReturnType = toPtr loop
                                    , ftName = "getLoopFor"
                                    , ftArgs = [(False,toConstPtr blk)]
                                    },GenHS,"loopInfoBaseGetLoopFor_")]
           }]
     | (blk,loop) <- [(normalT $ llvmType "BasicBlock",normalT $ llvmType "Loop")]
    ]++
    [Spec { specHeader = "llvm/PassAnalysisSupport.h"
          , specNS = llvmNS
          , specName = "AnalysisUsage"
          , specTemplateArgs = []
          , specType = ClassSpec
                       [(Constructor [],GenHS,"newAnalysisUsage")
                       ,(memberFun { ftName = "addRequiredID"
                                   , ftArgs = [(False,normalT $ ref char)]
                                   , ftIgnoreReturn = True
                                   },GenHS,"analysisUsageAddRequired_")
                       ,(memberFun { ftName = "addRequiredTransitiveID"
                                   , ftArgs = [(False,normalT $ ref char)]
                                   , ftIgnoreReturn = True
                                   },GenHS,"analysisUsageAddRequiredTransitive_")
                       ,(memberFun { ftName = "addPreservedID"
                                   , ftArgs = [(False,normalT $ ref char)]
                                   , ftIgnoreReturn = True
                                   },GenHS,"analysisUsageAddPreserved_")
                       ,(memberFun { ftName = "setPreservesAll"
                                   },GenHS,"analysisUsagePreservesAll")
                       ,(memberFun { ftName = "setPreservesCFG"
                                   },GenHS,"analysisUsagePreservesCFG")]
          }
    ,Spec { specHeader = "llvm/PassAnalysisSupport.h"
          , specNS = llvmNS
          , specName = "AnalysisResolver"
          , specTemplateArgs = []
          , specType = ClassSpec
                       [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Pass"
                                   , ftName = "findImplPass"
                                   , ftArgs = [(False,constT $ ptr void)]
                                   },GenHS,"analysisResolverFindImplPass_")
                       ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Pass"
                                   , ftName = "findImplPass"
                                   , ftArgs = [(True,normalT $ ptr $ llvmType "Pass")
                                              ,(False,constT $ ptr void)
                                              ,(False,normalT $ ref $ llvmType "Function")]
                                   },GenHS,"analysisResolverFindImplPassFun_")]
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/GenericValue.h"
          , specNS = llvmNS
          , specName = "GenericValue"
          , specTemplateArgs = []
          , specType = ClassSpec $
                       [(Constructor [],GenHS,"newGenericValue")
                       ,(Getter { ftGetVar = "DoubleVal"
                                , ftGetType = normalT double
                                , ftGetStatic = False
                                },GenHS,"genericValueGetDouble")
                       ,(Setter { ftSetVar = "DoubleVal"
                                , ftSetType = normalT double
                                },GenHS,"genericValueSetDouble")
                       ,(Getter { ftGetVar = "FloatVal"
                                , ftGetType = normalT float
                                , ftGetStatic = False
                                },GenHS,"genericValueGetFloat")
                       ,(Setter { ftSetVar = "FloatVal"
                                , ftSetType = normalT float
                                },GenHS,"genericValueSetFloat")
                       ,(Getter { ftGetVar = "PointerVal"
                                , ftGetType = normalT $ ptr void
                                , ftGetStatic = False
                                },GenHS,"genericValueGetPointer")
                       ,(Setter { ftSetVar = "PointerVal"
                                , ftSetType = normalT $ ptr void
                                },GenHS,"genericValueSetPointer")
                       ,(Getter { ftGetVar = "IntVal"
                                , ftGetType = normalT $ llvmType "APInt"
                                , ftGetStatic = False
                                },GenHS,"genericValueGetInt")
                       ,(Setter { ftSetVar = "IntVal"
                                , ftSetType = normalT $ llvmType "APInt"
                                },GenHS,"genericValueSetInt")]++
                       (if version>=llvm3_3
                        then [(Getter { ftGetVar = "AggregateVal"
                                      , ftGetType = normalT $ NamedType [ClassName "std" []] "vector"
                                                    [normalT $ llvmType "GenericValue"]
                                      , ftGetStatic = False
                                      },GenHS,"genericValueGetAggregate")
                             ,(Setter { ftSetVar = "AggregateVal"
                                      , ftSetType = normalT $ NamedType [ClassName "std" []] "vector"
                                                    [normalT $ llvmType "GenericValue"]
                                      },GenHS,"genericValueSetAggregate")]
                        else [])
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/ExecutionEngine.h"
          , specNS = llvmNS
          , specName = "ExecutionEngine"
          , specTemplateArgs = []
          , specType = ClassSpec $
                       [(Destructor True,GenHS,"deleteExecutionEngine_")
                       ,(memberFun { ftName = "addModule"
                                   , ftArgs = [(False,normalT $ ptr $ llvmType "Module")]
                                   , ftOverloaded = True
                                   },GenHS,"executionEngineAddModule_")
                       ,if version >= llvm3_2
                        then (memberFun { ftReturnType = normalT $ ptr $ llvmType "DataLayout"
                                        , ftName = "getDataLayout"
                                        , ftOverloaded = True
                                        },GenHS,"executionEngineGetDataLayout_")
                        else (memberFun { ftReturnType = normalT $ ptr $ llvmType "TargetData"
                                        , ftName = "getTargetData"
                                        , ftOverloaded = True
                                        },GenHS,"executionEngineGetTargetData_")
                       ,(memberFun { ftReturnType = normalT bool
                                   , ftName = "removeModule"
                                   , ftArgs = [(False,normalT $ ptr $ llvmType "Module")]
                                   , ftOverloaded = True
                                   },GenHS,"executionEngineRemoveModule_")
                       ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Function"
                                   , ftName = "FindFunctionNamed"
                                   , ftArgs = [(False,constT $ ptr char)]
                                   , ftOverloaded = True
                                   },GenHS,"executionEngineFindFunctionNamed_")
                       ,(memberFun { ftReturnType = normalT $ llvmType "GenericValue"
                                   , ftName = "runFunction"
                                   , ftArgs = [(False,normalT $ ptr $ llvmType "Function")
                                              ,(False,constT $ ref $ NamedType [ClassName "std" []] "vector"
                                                         [normalT $ llvmType "GenericValue"])]
                                   , ftOverloaded = True
                                   },GenHS,"executionEngineRunFunction_")]++
            (if version >= llvm3_1
             then [(memberFun { ftReturnType = normalT $ ptr void
                              , ftName = "getPointerToNamedFunction"
                              , ftArgs = [(False,constT $ ptr char)
                                         ,(False,normalT bool)]
                              , ftOverloaded = True
                              },GenHS,"executionEngineGetPointerToNamedFunction_")
                  ,(memberFun { ftName = "mapSectionAddress"
                        , ftArgs = [(False,if version >= llvm3_2
                                           then constT $ ptr void
                                           else normalT $ ptr void)
                                   ,(False,normalT uint64_t)]
                                   , ftOverloaded = True
                        },GenHS,"executionEngineMapSectionAddress_")]
             else [])++
            [(memberFun { ftName = "runStaticConstructorsDestructors"
                        , ftArgs = [(False,normalT bool)]
                        , ftOverloaded = True
                        },GenHS,"executionEngineRunStaticConstructorsDestructors_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToFunction"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Function")]
                        , ftOverloaded = True
                        },GenHS,"executionEngineGetPointerToFunction_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToFunctionOrStub"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Function")]
                        , ftOverloaded = True
                        },GenHS,"executionEngineGetPointerToFunctionOrStub_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToGlobal"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")]
                        , ftOverloaded = True
                        },GenHS,"executionEngineGetPointerToGlobal_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToGlobalIfAvailable"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")]
                        , ftOverloaded = True
                        },GenHS,"executionEngineGetPointerToGlobalIfAvailable_")
            ,(memberFun { ftName = "addGlobalMapping"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")
                                   ,(False,normalT $ ptr void)]
                        , ftOverloaded = True
                        },GenHS,"executionEngineAddGlobalMapping_")
            ,(memberFun { ftName = "clearAllGlobalMappings"
                        , ftOverloaded = True
                        },GenHS,"executionEngineClearAllGlobalMappings_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "updateGlobalMapping"
                        , ftArgs = [(True,constT $ ptr $ llvmType "GlobalValue")
                                   ,(False,normalT $ ptr void)]
                        , ftOverloaded = True
                        },GenHS,"executionEngineUpdateGlobalMapping_")
            ,(memberFun { ftReturnType = normalT $ ptr void
                        , ftName = "getPointerToBasicBlock"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "BasicBlock")]
                        , ftOverloaded = True
                        },GenHS,"executionEngineGetPointerToBasicBlock_")
            ,(memberFun { ftName = "runJITOnFunction"
                        , ftArgs = [(False,normalT $ ptr $ llvmType "Function")
                                   ,(False,normalT $ ptr $ llvmType "MachineCodeInfo")]
                        , ftOverloaded = True
                        },GenHS,"executionEngineRunJITOnFunction_")
            ,(memberFun { ftReturnType = constT $ ptr $ llvmType "GlobalValue"
                        , ftName = "getGlobalValueAtAddress"
                        , ftArgs = [(False,normalT $ ptr void)]
                        , ftOverloaded = True
                        },GenHS,"executionEngineGetGlobalValueAtAddress_")
            ,(memberFun { ftName = "StoreValueToMemory"
                        , ftArgs = [(False,constT $ ref $ llvmType "GenericValue")
                                   ,(False,normalT $ ptr $ llvmType "GenericValue")
                                   ,(True,normalT $ ptr $ llvmType "Type")]
                        , ftOverloaded = True
                        },GenHS,"executionEngineStoreValueToMemory_")
            ]
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/ExecutionEngine.h"
          , specNS = llvmNS
          , specName = "EngineBuilder"
          , specTemplateArgs = []
          , specType = ClassSpec
                       [(Constructor [(False,normalT $ ptr $ llvmType "Module")],GenHS,"newEngineBuilder")
                       ,(Destructor False,GenHS,"deleteEngineBuilder")
                       ,(memberFun { ftIgnoreReturn = True
                                   , ftName = "setEngineKind"
                                   , ftArgs = [(False,normalT $ EnumType [ClassName "llvm" []
                                                                         ,ClassName "EngineKind" []
                                                                         ] "Kind")]
                                   },GenHS,"engineBuilderSetKind")
                       ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ExecutionEngine"
                                   , ftName = "create"
                                   },GenHS,"engineBuilderCreate")
                       ]
          }
    ,Spec { specHeader = "llvm/ExecutionEngine/ExecutionEngine.h"
          , specNS = [ClassName "llvm" [],ClassName "EngineKind" []]
          , specName = "Kind"
          , specTemplateArgs = []
          , specType = EnumSpec "EngineKind" [("JIT","JIT")
                                             ,("Interpreter","Interpreter")
                                             ,("Either","EitherEngine")]
          }
    ,Spec { specHeader = "llvm/CodeGen/MachineCodeInfo.h"
          , specNS = llvmNS
          , specName = "MachineCodeInfo"
          , specTemplateArgs = []
          , specType = ClassSpec
                       [(Constructor [],GenHS,"newMachineCodeInfo")
                       ,(memberFun { ftName = "setSize"
                                   , ftArgs = [(False,normalT size_t)]
                                   },GenHS,"machineCodeInfoSetSize")
                       ,(memberFun { ftName = "setAddress"
                                   , ftArgs = [(False,normalT $ ptr void)]
                                   },GenHS,"machineCodeInfoSetAddress")
                       ,(memberFun { ftReturnType = normalT size_t
                                   , ftName = "size"
                                   },GenHS,"machineCodeInfoGetSize")
                       ,(memberFun { ftReturnType = normalT $ ptr void
                                   , ftName = "address"
                                   },GenHS,"machineCodeInfoGetAddress")]
          }
    ,Spec { specHeader = "llvm/Pass.h"
          , specNS = llvmNS
          , specName = "PassKind"
          , specTemplateArgs = []
          , specType = EnumSpec "PassKind" [("PT_"++name,"PassKind"++name)
                                            | name <- ["BasicBlock"]++
                                                     (if version>=llvm2_9
                                                      then ["Region"]
                                                      else [])++
                                                     ["Loop"
                                                     ,"Function"
                                                     ,"CallGraphSCC"
                                                     ,"Module"
                                                     ,"PassManager"]]
          }
    ,Spec { specHeader = irInclude version "InstrTypes.h"
          , specNS = [ClassName "llvm" [],ClassName "CmpInst" []]
          , specName = "Predicate"
          , specTemplateArgs = []
          , specType = EnumSpec "FCmpOp"
                       [("FCMP_"++name,"F_"++name)
                        | name <- ["OEQ","OGT","OGE","OLT"
                                 ,"OLE","ONE","ORD","UNO"
                                 ,"UEQ","UGT","UGE","ULT"
                                 ,"ULE","UNE"]]
          }
    ,Spec { specHeader = irInclude version "InstrTypes.h"
          , specNS = [ClassName "llvm" [],ClassName "CmpInst" []]
          , specName = "Predicate"
          , specTemplateArgs = []
          , specType = EnumSpec "ICmpOp"
                       [("ICMP_"++name,"I_"++name)
                        | name <- ["EQ","NE","UGT","UGE"
                                 ,"ULT","ULE","SGT","SGE"
                                 ,"SLT","SLE"]]
          }
    ,Spec { specHeader = irInclude version "CallingConv.h"
          , specNS = [ClassName "llvm" [],ClassName "CallingConv" []]
          , specName = "ID"
          , specTemplateArgs = []
          , specType = EnumSpec "CallingConv"
                       [(name,name)
                        | name <- ["C","Fast","Cold","GHC"
                                 ,"FirstTargetCC"
                                 ,"X86_StdCall","X86_FastCall"
                                 ,"ARM_APCS","ARM_AAPCS"
                                 ,"ARM_AAPCS_VFP"
                                 ,"MSP430_INTR"
                                 ,"X86_ThisCall"]++
                                 (if version>=llvm2_9
                                  then ["PTX_Kernel","PTX_Device"
                                       ,"MBLAZE_INTR","MBLAZE_SVOL"]
                                  else [])++
                                 (if version>llvm3_1
                                  then ["SPIR_FUNC"
                                       ,"SPIR_KERNEL"
                                       ,"Intel_OCL_BI"]
                                  else [])]
          }]++
    (if version>=llvm3_0
     then [Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "SynchronizationScope"
                , specTemplateArgs = []
                , specType = EnumSpec "SynchronizationScope"
                             [("SingleThread","SingleThread")
                             ,("CrossThread","CrossThread")]
                }
          ,Spec { specHeader = irInclude version "Instructions.h"
                , specNS = llvmNS
                , specName = "AtomicOrdering"
                , specTemplateArgs = []
                , specType = EnumSpec "AtomicOrdering"
                             [(name,name) | name <- ["NotAtomic"
                                                   ,"Unordered"
                                                   ,"Monotonic"
                                                   ,"Acquire"
                                                   ,"Release"
                                                   ,"AcquireRelease"
                                                   ,"SequentiallyConsistent"]]
                }
          ,Spec { specHeader = irInclude version "Instructions.h"
                , specNS = [ClassName "llvm" [],ClassName "AtomicRMWInst" []]
                , specName = "BinOp"
                , specTemplateArgs = []
                , specType = EnumSpec "RMWBinOp"
                             [(name,"RMW"++name)
                                  | name <- ["Xchg"
                                           ,"Add"
                                           ,"Sub"
                                           ,"And"
                                           ,"Nand"
                                           ,"Or"
                                           ,"Xor"
                                           ,"Max"
                                           ,"Min"
                                           ,"UMax"
                                           ,"UMin"]]
                }]
     else [])++
    [Spec { specHeader = "llvm/Analysis/AliasAnalysis.h"
          , specNS = [ClassName "llvm" [],ClassName "AliasAnalysis" []]
          , specName = "AliasResult"
          , specTemplateArgs = []
          , specType = EnumSpec "AliasResult" $
                       [("NoAlias","NoAlias")
                       ,("MayAlias","MayAlias")]++
                       (if version>=llvm2_9
                        then [("PartialAlias","PartialAlias")]
                        else [])++
                       [("MustAlias","MustAlias")]
          }]++
    (if version>=llvm2_9
     then [Spec { specHeader = "llvm/Target/TargetLibraryInfo.h"
                , specNS = [ClassName "llvm" [],ClassName "LibFunc" []]
                , specName = "Func"
                , specTemplateArgs = []
                , specType = EnumSpec "LibFunc"
                             [(name,"Func_"++name)
                              | name <- ["fiprintf"
                                       ,"iprintf"
                                       ,"memcpy"
                                       ,"memset"
                                       ,"memset_pattern16"
                                       ,"siprintf"]++
                                       (if version>=llvm3_0
                                        then ["memmove"]
                                        else [])++
                                       (if version>=llvm3_1
                                        then ["cxa_atexit"
                                             ,"cxa_guard_abort"
                                             ,"cxa_guard_acquire"
                                             ,"cxa_guard_release"
                                             ,"acos"
                                             ,"acosf"
                                             ,"acosl"
                                             ,"asin"
                                             ,"asinf"
                                             ,"asinl"
                                             ,"atan"
                                             ,"atan2"
                                             ,"atan2f"
                                             ,"atan2l"
                                             ,"atanf"
                                             ,"atanl"
                                             ,"ceil"
                                             ,"ceilf"
                                             ,"ceill"
                                             ,"copysign"
                                             ,"copysignf"
                                             ,"copysignl"
                                             ,"cos"
                                             ,"cosf"
                                             ,"cosh"
                                             ,"coshf"
                                             ,"coshl"
                                             ,"cosl"
                                             ,"exp"
                                             ,"exp2"
                                             ,"exp2f"
                                             ,"exp2l"
                                             ,"expf"
                                             ,"expl"
                                             ,"expm1"
                                             ,"expm1f"
                                             ,"expm1l"
                                             ,"fabs"
                                             ,"fabsf"
                                             ,"fabsl"
                                             ,"floor"
                                             ,"floorf"
                                             ,"floorl"
                                             ,"fmod"
                                             ,"fmodf"
                                             ,"fmodl"
                                             ,"fputs"
                                             ,"fwrite"
                                             ,"log"
                                             ,"log10"
                                             ,"log10f"
                                             ,"log10l"
                                             ,"log1p"
                                             ,"log1pf"
                                             ,"log1pl"
                                             ,"log2"
                                             ,"log2f"
                                             ,"log2l"
                                             ,"logf"
                                             ,"logl"
                                             ,"nearbyint"
                                             ,"nearbyintf"
                                             ,"nearbyintl"
                                             ,"pow"
                                             ,"powf"
                                             ,"powl"
                                             ,"rint"
                                             ,"rintf"
                                             ,"rintl"
                                             ,"round"
                                             ,"roundf"
                                             ,"roundl"
                                             ,"sin"
                                             ,"sinf"
                                             ,"sinh"
                                             ,"sinhf"
                                             ,"sinhl"
                                             ,"sinl"
                                             ,"sqrt"
                                             ,"sqrtf"
                                             ,"sqrtl"
                                             ,"tan"
                                             ,"tanf"
                                             ,"tanh"
                                             ,"tanhf"
                                             ,"tanhl"
                                             ,"tanl"
                                             ,"trunc"
                                             ,"truncf"
                                             ,"truncl"]++
                                        (if version>=llvm3_3
                                         then ["under_IO_getc"
                                              ,"under_IO_putc"
                                              ,"ZdaPv"
                                              ,"ZdlPv"
                                              ,"Znaj"
                                              ,"ZnajRKSt9nothrow_t"
                                              ,"Znam"
                                              ,"ZnamRKSt9nothrow_t"
                                              ,"Znwj"
                                              ,"ZnwjRKSt9nothrow_t"
                                              ,"Znwm"
                                              ,"ZnwmRKSt9nothrow_t"
                                              ,"dunder_isoc99_scanf"
                                              ,"dunder_isoc99_sscanf"
                                              ,"memcpy_chk"
                                              ,"dunder_strdup"
                                              ,"dunder_strndup"
                                              ,"dunder_strtok_r"
                                              ,"abs"
                                              ,"access"
                                              ,"acosh"
                                              ,"acoshf"
                                              ,"acoshl"
                                              ,"asinh"
                                              ,"asinhf"
                                              ,"asinhl"
                                              ,"atanh"
                                              ,"atanhf"
                                              ,"atanhl"
                                              ,"atof"
                                              ,"atoi"
                                              ,"atol"
                                              ,"atoll"
                                              ,"bcmp"
                                              ,"bcopy"
                                              ,"bzero"
                                              ,"calloc"
                                              ,"cbrt"
                                              ,"cbrtf"
                                              ,"cbrtl"
                                              ,"chmod"
                                              ,"chown"
                                              ,"clearerr"
                                              ,"closedir"
                                              ,"ctermid"
                                              ,"exp10"
                                              ,"exp10f"
                                              ,"exp10l"
                                              ,"fclose"
                                              ,"fdopen"
                                              ,"feof"
                                              ,"ferror"
                                              ,"fflush"
                                              ,"ffs"
                                              ,"ffsl"
                                              ,"ffsll"
                                              ,"fgetc"
                                              ,"fgetpos"
                                              ,"fgets"
                                              ,"fileno"
                                              ,"flockfile"
                                              ,"fopen"
                                              ,"fopen64"
                                              ,"fprintf"
                                              ,"fputc"
                                              ,"fread"
                                              ,"free"
                                              ,"frexp"
                                              ,"frexpf"
                                              ,"frexpl"
                                              ,"fscanf"
                                              ,"fseek"
                                              ,"fseeko"
                                              ,"fseeko64"
                                              ,"fsetpos"
                                              ,"fstat"
                                              ,"fstat64"
                                              ,"fstatvfs"
                                              ,"fstatvfs64"
                                              ,"ftell"
                                              ,"ftello"
                                              ,"ftello64"
                                              ,"ftrylockfile"
                                              ,"funlockfile"
                                              ,"getc"
                                              ,"getc_unlocked"
                                              ,"getchar"
                                              ,"getenv"
                                              ,"getitimer"
                                              ,"getlogin_r"
                                              ,"getpwnam"
                                              ,"gets"
                                              ,"htonl"
                                              ,"htons"
                                              ,"isascii"
                                              ,"isdigit"
                                              ,"labs"
                                              ,"lchown"
                                              ,"llabs"
                                              ,"logb"
                                              ,"logbf"
                                              ,"logbl"
                                              ,"lstat"
                                              ,"lstat64"
                                              ,"malloc"
                                              ,"memalign"
                                              ,"memccpy"
                                              ,"memchr"
                                              ,"memcmp"
                                              ,"memrchr"
                                              ,"mkdir"
                                              ,"mktime"
                                              ,"modf"
                                              ,"modff"
                                              ,"modfl"
                                              ,"ntohl"
                                              ,"ntohs"
                                              ,"open"
                                              ,"open64"
                                              ,"opendir"
                                              ,"pclose"
                                              ,"perror"
                                              ,"popen"
                                              ,"posix_memalign"
                                              ,"pread"
                                              ,"printf"
                                              ,"putc"
                                              ,"putchar"
                                              ,"puts"
                                              ,"pwrite"
                                              ,"qsort"
                                              ,"read"
                                              ,"readlink"
                                              ,"realloc"
                                              ,"reallocf"
                                              ,"realpath"
                                              ,"remove"
                                              ,"rename"
                                              ,"rewind"
                                              ,"rmdir"
                                              ,"scanf"
                                              ,"setbuf"
                                              ,"setitimer"
                                              ,"setvbuf"
                                              ,"snprintf"
                                              ,"sprintf"
                                              ,"sscanf"
                                              ,"stat"
                                              ,"stat64"
                                              ,"statvfs"
                                              ,"statvfs64"
                                              ,"stpcpy"
                                              ,"stpncpy"
                                              ,"strcasecmp"
                                              ,"strcat"
                                              ,"strchr"
                                              ,"strcmp"
                                              ,"strcoll"
                                              ,"strcpy"
                                              ,"strcspn"
                                              ,"strdup"
                                              ,"strlen"
                                              ,"strncasecmp"
                                              ,"strncat"
                                              ,"strncmp"
                                              ,"strncpy"
                                              ,"strndup"
                                              ,"strnlen"
                                              ,"strpbrk"
                                              ,"strrchr"
                                              ,"strspn"
                                              ,"strstr"
                                              ,"strtod"
                                              ,"strtof"
                                              ,"strtok"
                                              ,"strtok_r"
                                              ,"strtol"
                                              ,"strtold"
                                              ,"strtoll"
                                              ,"strtoul"
                                              ,"strtoull"
                                              ,"strxfrm"
                                              ,"system"
                                              ,"times"
                                              ,"tmpfile"
                                              ,"tmpfile64"
                                              ,"toascii"
                                              ,"uname"
                                              ,"ungetc"
                                              ,"unlink"
                                              ,"unsetenv"
                                              ,"utime"
                                              ,"utimes"
                                              ,"valloc"
                                              ,"vfprintf"
                                              ,"vfscanf"
                                              ,"vprintf"
                                              ,"vscanf"
                                              ,"vsnprintf"
                                              ,"vsprintf"
                                              ,"vsscanf"
                                              ,"write"]
                                         else[])
                                        else [])
                             ]
                }
          ]
      else [])