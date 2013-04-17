module LLVMSpec where

import Data.Version
import Generator
import CPPType

llvm3_3 :: Version
llvm3_3 = Version { versionBranch = [3,3]
                  , versionTags = []
                  }

llvm3_1 :: Version
llvm3_1 = Version { versionBranch = [3,1]
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
    [Spec { specHeader = "llvm/ADT/ArrayRef.h"
          , specNS = llvmNS
          , specName = "ArrayRef"
          , specTemplateArgs = [rtp]
          , specType = ClassSpec
                       [(Constructor { ftConArgs = [] },GenHS,"newArrayRefEmpty"++tp)
                       ,(Constructor { ftConArgs = [(False,toPtr rtp)
                                                   ,(False,normalT size_t)] },GenHS,"newArrayRef"++tp)
                       ,(Destructor False,GenHS,"deleteArrayRef"++tp)
                       ,(memberFun { ftReturnType = normalT size_t
                                   , ftName = "size"
                                   },GenHS,"arrayRefSize"++tp)
                       ,(memberFun { ftReturnType = normalT bool
                                   , ftName = "equals"
                                   , ftArgs = [(False,normalT $ NamedType llvmNS "ArrayRef" [rtp])]
                                   },GenHS,"arrayRefEquals"++tp)
                       ,(memberFun { ftReturnType = toConstRef rtp
                                   , ftName = "operator[]"
                                   , ftArgs = [(False,normalT size_t)]
                                   },GenHS,"arrayRefIndex"++tp)
                       ]
          }
     | (tp,rtp) <- [("Type",normalT $ ptr $ llvmType "Type")
                  ,("CChar",constT $ ptr char)]
    ]++
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
                  , specNS = [ClassName "std" [],ClassName "vector" [rtp]]
                  , specName = "const_iterator"
                  , specTemplateArgs = []
                  , specType = ClassSpec
                               [(memberFun { ftReturnType = rtp
                                           , ftName = "operator*"
                                           },GenHS,"setVectorIterator"++tp++"Deref")
                               ,(memberFun { ftReturnType = normalT $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" []
                                           , ftName = "operator++"
                                           },GenHS,"setVectorIterator"++tp++"Next")
                               ]
                  }
            ,Spec { specHeader = "vector"
                  , specNS = []
                  , specName = "operator=="
                  , specTemplateArgs = []
                  , specType = GlobalFunSpec { gfunReturnType = normalT bool
                                             , gfunArgs = [(False,constT $ ref $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [])
                                                          ,(False,constT $ ref $ NamedType [ClassName "std" [],ClassName "vector" [rtp]] "const_iterator" [])]
                                             , gfunHSName = "setVectorIterator"++tp++"Eq"
                                             }
                  }
            ]
            | tp <- ["Type"]
           , let rtp = Type [] (ptr $ llvmType tp)
           ]++
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
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getBitWidth"
                                      },GenHS,"apIntGetBitWidth_")
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
             , specType = ClassSpec
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
                                      },GenHS,"debugLocGetInlinedAt")
                          ,(memberFun { ftName = "dump" 
                                      , ftArgs = [(False,constT $ ref $ NamedType llvmNS "LLVMContext" [])]
                                      },GenHS,"debugLocDump")
                          ]
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
                                  ,"FloatingPoint","X86_MMX","Label","Metadata"]
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
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isPacked"
                                      },GenHS,"structTypeIsPacked")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "hasName"
                                      },GenHS,"structTypeHasName")
                          ,(memberFun { ftReturnType = normalT $ NamedType llvmNS "StringRef" []
                                      , ftName = "getName"
                                      },GenHS,"structTypeGetName")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumElements"
                                      },GenHS,"structTypeGetNumElements_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "Type" []
                                      , ftName = "getElementType"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"structTypeGetElementType_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,normalT $ NamedType llvmNS "ArrayRef"
                                                        [normalT $ ptr $ llvmType "Type"])
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
                                                 ,(False,normalT $ NamedType llvmNS "ArrayRef" 
                                                            [normalT $ ptr $ llvmType "Type"])
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
                                      },GenHS,"globalValueGetType")]
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
             }
       ,Spec { specHeader = "llvm/Support/system_error.h"
             , specNS = llvmNS
             , specName = "error_code"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor False,GenHS,"deleteErrorCode")
                          ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                      , ftName = "value"
                                      },GenHS,"errorCodeValue_")]
             }
       ,Spec { specHeader = "llvm/Support/MemoryBuffer.h"
             , specNS = llvmNS
             , specName = "MemoryBuffer"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor False,GenHS,"deleteMemoryBuffer")
                          ,(memberFun { ftReturnType = normalT size_t 
                                      , ftName = "getBufferSize"
                                      },GenHS,"getBufferSize_")
                          ,(memberFun { ftReturnType = normalT (NamedType llvmNS "error_code" []) 
                                      , ftName = "getFile"
                                      , ftArgs = [(False,normalT (NamedType llvmNS "StringRef" []))
                                                 ,(False,normalT (RefType $ NamedType llvmNS "OwningPtr" 
                                                                  [normalT (NamedType llvmNS "MemoryBuffer" [])]))
                                                 ,(False,normalT (NamedType [] "int64_t" []))
                                                 ,(False,normalT (NamedType [] "bool" []))]
                                      , ftStatic = True
                                      },GenHS,"getFileMemoryBuffer")]
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
                                                      ,"Instruction"
                                                      ,"AtomicCmpXchgInst"
                                                      ,"AtomicRMWInst"
                                                      ,"BinaryOperator"
                                                      ,"CallInst"
                                                      ,"CmpInst"
                                                      ,"FCmpInst"
                                                      ,"ICmpInst"
                                                      ,"ExtractElementInst"
                                                      ,"FenceInst"
                                                      ,"GetElementPtrInst"
                                                      ,"InsertElementInst"
                                                      ,"InsertValueInst"
                                                      ,"LandingPadInst"
                                                      ,"PHINode"
                                                      ,"SelectInst"
                                                      ,"ShuffleVectorInst"
                                                      ,"StoreInst"
                                                      ,"TerminatorInst"
                                                      ,"BranchInst"
                                                      ,"IndirectBrInst"
                                                      ,"InvokeInst"
                                                      ,"ResumeInst"
                                                      ,"ReturnInst"
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
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
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
                                        ,(False,normalT $ ptr $ llvmType "BasicBlock")
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
                                      },GenHS,"atomicRMWInstGetValOperand")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "BinaryOperator"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getOpcode"
                                      },GenHS,"binOpGetOpCode_")]
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
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "ICmpInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
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
                                      },GenHS,"extractElementInstGetIndexOperand")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "FenceInst"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getOrdering"
                                      },GenHS,"fenceInstGetOrdering_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
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
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "InsertElementInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType llvmNS "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"insertElementInstGetType")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = llvmNS
             , specName = "InsertValueInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
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
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
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
                                      },GenHS,"phiNodeGetIncomingBlock_")]
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
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },GenHS,"storeInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },GenHS,"storeInstGetAlignment_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getOrdering"
                                      },GenHS,"storeInstGetOrdering_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
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
             , specType = ClassSpec
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
                                      },GenHS,"invokeInstGetUnwindDest")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "LandingPadInst"
                                      , ftName = "getLandingPadInst"
                                      },GenHS,"invokeInstGetLandingPadInst")
                          ]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = llvmNS
             , specName = "ResumeInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
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
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },GenHS,"loadInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },GenHS,"loadInstGetAlignment_")
                          ,(memberFun { ftReturnType = normalT int
                                      , ftName = "getOrdering"
                                      },GenHS,"loadInstGetOrdering_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
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
                           ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = llvmNS
             , specName = "FunctionPass"
             , specTemplateArgs = []
             , specType = ClassSpec []
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
             }
       ,Spec { specHeader = "llvm/Target/TargetLibraryInfo.h"
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
             }]++
       (if version >= llvm3_3 
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
       ,Spec { specHeader = "llvm/Transforms/IPO/PassManagerBuilder.h"
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
             }
       ]++
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
                           ,("ModulePass","createInternalizePass",[normalT $ NamedType llvmNS "ArrayRef" [constT $ ptr $ char]])
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
                          [(memberFun { ftReturnType = normalT $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" []
                                      , ftName = "getLocation"
                                      , ftArgs = [(False,constT $ ptr $ llvmType (inst++"Inst"))]
                                      , ftOverloaded = True
                                      },GenHS,"aliasAnalysisGetLocation"++inst++"_")
                           | inst <- ["Load","Store","VAArg","AtomicCmpXchg","AtomicRMW"]]++
                          [(memberFun { ftReturnType = normalT $ EnumType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "AliasResult"
                                      , ftName = "alias"
                                      , ftArgs = [(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" [])
                                                 ,(False,constT $ ref $ NamedType [ClassName "llvm" [],ClassName "AliasAnalysis" []] "Location" [])]
                                      , ftOverloaded = True
                                      },GenHS,"aliasAnalysisAlias_")
                          ]
             }
       ,Spec { specHeader = "llvm/Analysis/AliasAnalysis.h"
             , specNS = [ClassName "llvm" [],ClassName "AliasAnalysis" []]
             , specName = "Location"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [(True,constT $ ptr $ llvmType "Value")
                                        ,(False,normalT uint64_t)
                                        ,(False,constT $ ptr $ llvmType "MDNode")
                                        ],GenHS,"newLocation_")]
             }
       ,Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = "getMallocAllocatedType"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "Type"
                                        , gfunArgs = [(False,constT $ ptr $ llvmType "CallInst")]++
                                                     (if version>=llvm3_3
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
                                                     (if version >= llvm3_3
                                                      then [(False,constT $ ptr $ llvmType "DataLayout")
                                                           ,(False,constT $ ptr $ llvmType "TargetLibraryInfo")]
                                                      else [(False,constT $ ptr $ llvmType "TargetData")])++
                                                     [(False,normalT bool)]
                                        , gfunHSName = "getMallocArraySize"
                                        }
             }
       ,Spec { specHeader = "llvm/Analysis/MemoryBuiltins.h"
             , specNS = llvmNS
             , specName = if version>=llvm3_3
                          then "isMallocLikeFn"
                          else "isMalloc"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT bool
                                        , gfunArgs = [(True,constT $ ptr $ llvmType "Value")]++
                                                     (if version>=llvm3_3
                                                      then [(False,constT $ ptr $ llvmType "TargetLibraryInfo")
                                                           ,(False,normalT bool)]
                                                      else [])
                                        , gfunHSName = "isMallocLikeFn_"
                                        }
             }
       ]