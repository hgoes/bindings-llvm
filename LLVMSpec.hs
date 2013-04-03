module LLVMSpec where

import Data.Version
import Generator
import CPPType

llvm3_3 :: Version
llvm3_3 = Version { versionBranch = [3,3]
                  , versionTags = []
                  }

irInclude :: Version -> String -> String
irInclude ver hdr = if ver >= llvm3_3
                    then "llvm/IR/"++hdr
                    else "llvm/"++hdr

llvm :: Version -> [Spec]
llvm version
  = [Spec { specHeader = "llvm/ADT/StringRef.h"
          , specNS = ["llvm"]
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
             , specNS = ["llvm"]
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
       , let rtp = Type [] (NamedType ["llvm"] tp [])
       ]++
       [Spec { specHeader = "llvm/ADT/ArrayRef.h"
             , specNS = ["llvm"]
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
                                      , ftArgs = [(False,normalT $ NamedType ["llvm"] "ArrayRef" [rtp])]
                                      },GenHS,"arrayRefEquals"++tp)
                          ,(memberFun { ftReturnType = toConstRef rtp
                                      , ftName = "operator[]"
                                      , ftArgs = [(False,normalT size_t)]
                                      },GenHS,"arrayRefIndex"++tp)
                          ]
             }
       | tp <- ["Type"]
       , let rtp = Type [] (ptr $ NamedType ["llvm"] tp [])
       ]++
       concat [[Spec { specHeader = "llvm/ADT/ilist.h"
                     , specNS = ["llvm"]
                     , specName = "iplist"
                     , specTemplateArgs = [rtp]
                     , specType = ClassSpec
                                  [(Constructor { ftConArgs = [] },GenHS,"new"++tp++"List")
                                  ,(Destructor False,GenHS,"delete"++tp++"List")
                                  ,(memberFun { ftReturnType = normalT size_t
                                              , ftName = "size"
                                              },GenHS,"list"++tp++"Size")
                                  ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "ilist_iterator" [rtp]
                                              , ftName = "begin"
                                              },GenHS,"list"++tp++"Begin")
                                  ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "ilist_iterator" [rtp]
                                              , ftName = "end"
                                              },GenHS,"list"++tp++"End")
                                  ]
                     }
               ,Spec { specHeader = "llvm/ADT/ilist.h"
                     , specNS = ["llvm"]
                     , specName = "ilist_iterator"
                     , specTemplateArgs = [rtp]
                     , specType = ClassSpec
                                  [(memberFun { ftReturnType = toPtr rtp
                                              , ftName = "operator->"
                                              },GenHS,"listIterator"++tp++"Deref")
                                  ,(memberFun { ftReturnType = normalT $ RefType $ 
                                                               NamedType ["llvm"] "ilist_iterator" [rtp]
                                              , ftName = "operator++"
                                              },GenHS,"listIterator"++tp++"Next")
                                  ,(memberFun { ftReturnType = normalT bool
                                              , ftName = "operator=="
                                              , ftArgs = [(False,constT $ RefType $ 
                                                                NamedType ["llvm"] "ilist_iterator" [rtp])] 
                                              },GenHS,"listIterator"++tp++"Eq")
                                  ,(memberFun { ftReturnType = normalT bool
                                              , ftName = "operator!="
                                              , ftArgs = [(False,constT $ RefType $ 
                                                                NamedType ["llvm"] "ilist_iterator" [rtp])] 
                                              },GenHS,"listIterator"++tp++"NEq")]
                     }
               ]
              | tp <- ["Function","Instruction","BasicBlock","GlobalVariable","Argument"]
              , let rtp = Type [] (NamedType ["llvm"] tp [])
              ]++
       [Spec { specHeader = "llvm/ADT/APFloat.h"
             , specNS = ["llvm"]
             , specName = "APFloat"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT double
                                      , ftName = "convertToDouble"
                                      },GenHS,"apFloatConvertToDouble")]
             }
       ,Spec { specHeader = "llvm/ADT/APInt.h"
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
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
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "MDNode" []
                                      , ftName = "getScope"
                                      , ftArgs = [(False,constT $ ref $ NamedType ["llvm"] "LLVMContext" [])]
                                      },GenHS,"debugLocGetScope")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "MDNode" []
                                      , ftName = "getInlinedAt"
                                      , ftArgs = [(False,constT $ ref $ NamedType ["llvm"] "LLVMContext" [])]
                                      },GenHS,"debugLocGetInlinedAt")
                          ,(memberFun { ftName = "dump" 
                                      , ftArgs = [(False,constT $ ref $ NamedType ["llvm"] "LLVMContext" [])]
                                      },GenHS,"debugLocDump")
                          ]
             }
       ]++
       [Spec { specHeader = irInclude version "Type.h"
             , specNS = ["llvm"]
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
                           | tp <- ["Void","Half","Float","Double","X86_FP80","FP128","PPC_FP128"
                                  ,"FloatingPoint","X86_MMX","Label","Metadata"]
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = ["llvm"]
             , specName = "IntegerType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getBitWidth"
                                      },GenHS,"getBitWidth_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "IntegerType" [] 
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ RefType $ NamedType ["llvm"] "LLVMContext" [])
                                                 ,(False,normalT $ NamedType [] "unsigned" [])]
                                      , ftStatic = True
                                      },GenHS,"getIntegerType_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = ["llvm"]
             , specName = "CompositeType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
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
             , specNS = ["llvm"]
             , specName = "SequentialType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
                                      , ftName = "getElementType"
                                      , ftOverloaded = True
                                      },GenHS,"sequentialTypeGetElementType_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = ["llvm"]
             , specName = "ArrayType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT uint64_t
                                      , ftName = "getNumElements"
                                      },GenHS,"arrayTypeGetNumElements_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = ["llvm"]
             , specName = "PointerType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAddressSpace"
                                      },GenHS,"pointerTypeGetAddressSpace_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ NamedType ["llvm"] "Type" [])
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },GenHS,"pointerTypeGet_")]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = ["llvm"]
             , specName = "VectorType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumElements"
                                      },GenHS,"vectorTypeGetNumElements_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ NamedType ["llvm"] "Type" [])
                                                 ,(False,normalT unsigned)]
                                      , ftStatic = True
                                      },GenHS,"vectorTypeGet_")
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = ["llvm"]
             , specName = "StructType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isPacked"
                                      },GenHS,"structTypeIsPacked")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "hasName"
                                      },GenHS,"structTypeHasName")
                          ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "StringRef" []
                                      , ftName = "getName"
                                      },GenHS,"structTypeGetName")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumElements"
                                      },GenHS,"structTypeGetNumElements_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
                                      , ftName = "getElementType"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"structTypeGetElementType_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "StructType"
                                      , ftName = "get"
                                      , ftArgs = [(False,normalT $ ref $ llvmType "LLVMContext")
                                                 ,(False,normalT $ NamedType ["llvm"] "ArrayRef"
                                                        [normalT $ ptr $ llvmType "Type"])
                                                 ,(False,normalT bool)]
                                      , ftStatic = True
                                      },GenHS,"newStructType")
                          ]
             }
       ,Spec { specHeader = irInclude version "DerivedTypes.h"
             , specNS = ["llvm"]
             , specName = "FunctionType"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVarArg"
                                      },GenHS,"functionTypeIsVarArg")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumParams"
                                      },GenHS,"functionTypeGetNumParams_")
                          ,(memberFun { ftReturnType = normalT (ptr $ NamedType ["llvm"] "Type" [])
                                      , ftName = "getParamType"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"functionTypeGetParamType_")
                          ,(memberFun { ftReturnType = normalT (ptr $ NamedType ["llvm"] "Type" [])
                                      , ftName = "getReturnType"
                                      },GenHS,"functionTypeGetReturnType")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionType"
                                      , ftName = "get"
                                      , ftArgs = [(True,normalT $ ptr $ llvmType "Type")
                                                 ,(False,normalT $ NamedType ["llvm"] "ArrayRef" 
                                                            [normalT $ ptr $ llvmType "Type"])
                                                 ,(False,normalT bool)
                                                 ]
                                      , ftStatic = True
                                      },GenHS,"newFunctionType_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Value.h"
             , specNS = ["llvm"]
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
                          ,(memberFun { ftReturnType = normalT $ NamedType ["llvm"] "StringRef" []
                                      , ftName = "getName"
                                      , ftOverloaded = True
                                      },GenHS,"getName_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Type" []
                                      , ftName = "getType"
                                      , ftOverloaded = True
                                      },GenHS,"valueGetType_")
                          ]
             }
       ,Spec { specHeader = irInclude version "Argument.h"
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
             , specName = "BasicBlock"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Function" []
                                      , ftName = "getParent"
                                      },GenHS,"basicBlockGetParent")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "iplist" 
                                                       [normalT $ NamedType ["llvm"] "Instruction" []]
                                      , ftName = "getInstList"
                                      },GenHS,"getInstList")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "TerminatorInst" []
                                      , ftName = "getTerminator"
                                      },GenHS,"getTerminator")
                          ]
             }
       ,Spec { specHeader = irInclude version "InlineAsm.h"
             , specNS = ["llvm"]
             , specName = "InlineAsm"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Metadata.h"
             , specNS = ["llvm"]
             , specName = "MDNode"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Metadata.h"
             , specNS = ["llvm"]
             , specName = "MDString"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = "llvm/CodeGen/PseudoSourceValue.h"
             , specNS = ["llvm"]
             , specName = "FixedStackPseudoSourceValue"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Constant.h"
             , specNS = ["llvm"]
             , specName = "Constant"
             , specTemplateArgs = []
             , specType = ClassSpec 
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
             , specNS = ["llvm"]
             , specName = "BlockAddress"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Constant"
                                      , ftName = "getAggregateElement"
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },GenHS,"constantGetAggregateElement_")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantArray"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "ArrayType" []
                                      , ftName = "getType"
                                      },GenHS,"constantArrayGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantDataSequential"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "SequentialType" []
                                      , ftName = "getType"
                                      , ftOverloaded = True
                                      },GenHS,"constantDataSequentialGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantDataArray"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "ArrayType" []
                                      , ftName = "getType"
                                      },GenHS,"constantDataArrayGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantDataVector"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"constantDataVectorGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantExpr"
             , specTemplateArgs = []
             , specType = ClassSpec [] --[(memberFun { ftReturnType = normalT $ ptr $ llvmType "Instruction"
                                        --            , ftName = "getAsInstruction"
                                        --            },GenHS,"constantExprGetAsInstruction")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantFP"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = constT $ ref $ llvmType "APFloat"
                                      , ftName = "getValueAPF"
                                      },GenHS,"constantFPGetValueAPF")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantInt"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "IntegerType" []
                                      , ftName = "getType"
                                      },GenHS,"constantIntGetType")
                          ,(memberFun { ftReturnType = constT $ ref $ llvmType "APInt"
                                      , ftName = "getValue"
                                      },GenHS,"constantIntGetValue")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantPointerNull"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                      , ftName = "getType"
                                      },GenHS,"constantPointerNullGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantStruct"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "StructType" []
                                      , ftName = "getType"
                                      },GenHS,"constantStructGetType")]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantVector"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"constantVectorGetType")]
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = ["llvm"]
             , specName = "GlobalValue"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
                                      , ftName = "getType"
                                      , ftOverloaded = True
                                      },GenHS,"globalValueGetType")]
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = ["llvm"]
             , specName = "GlobalAlias"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "GlobalValue.h"
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
             , specName = "Function"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVarArg"
                                      },GenHS,"functionIsVarArg")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "iplist" 
                                                       [normalT $ NamedType ["llvm"] "BasicBlock" []]
                                      , ftName = "getBasicBlockList"
                                      },GenHS,"getBasicBlockList")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "BasicBlock" []
                                      , ftName = "getEntryBlock"
                                      },GenHS,"getEntryBlock")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "FunctionType"
                                      , ftName = "getFunctionType"
                                      },GenHS,"functionGetFunctionType")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType ["llvm"] "iplist"
                                                       [normalT $ llvmType "Argument"]
                                      , ftName = "getArgumentList"
                                      },GenHS,"functionGetArgumentList")
                          ]
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "UndefValue"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = "llvm/Support/system_error.h"
             , specNS = ["llvm"]
             , specName = "error_code"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor False,GenHS,"deleteErrorCode")
                          ,(memberFun { ftReturnType = normalT (NamedType [] "int" [])
                                      , ftName = "value"
                                      },GenHS,"errorCodeValue_")]
             }
       ,Spec { specHeader = "llvm/Support/MemoryBuffer.h"
             , specNS = ["llvm"]
             , specName = "MemoryBuffer"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Destructor False,GenHS,"deleteMemoryBuffer")
                          ,(memberFun { ftReturnType = normalT size_t 
                                      , ftName = "getBufferSize"
                                      },GenHS,"getBufferSize_")
                          ,(memberFun { ftReturnType = normalT (NamedType ["llvm"] "error_code" []) 
                                      , ftName = "getFile"
                                      , ftArgs = [(False,normalT (NamedType ["llvm"] "StringRef" []))
                                                 ,(False,normalT (RefType $ NamedType ["llvm"] "OwningPtr" 
                                                                  [normalT (NamedType ["llvm"] "MemoryBuffer" [])]))
                                                 ,(False,normalT (NamedType [] "int64_t" []))
                                                 ,(False,normalT (NamedType [] "bool" []))]
                                      , ftStatic = True
                                      },GenHS,"getFileMemoryBuffer")]
             }
       ,Spec { specHeader = "llvm/Support/SourceMgr.h"
             , specNS = ["llvm"]
             , specName = "SMDiagnostic"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [],GenHS,"newSMDiagnostic")
                          ,(Destructor False,GenHS,"deleteSMDiagnostic")
                          ,(memberFun { ftReturnType = normalT (NamedType ["llvm"] "StringRef" [])
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
             , specNS = ["llvm"]
             , specName = "LLVMContext"
             , specTemplateArgs = []
             , specType = ClassSpec [(Constructor [],GenHS,"newLLVMContext")
                                    ,(Destructor False,GenHS,"deleteLLVMContext")]
             }
       ,Spec { specHeader = irInclude version "LLVMContext.h"
             , specNS = ["llvm"]
             , specName = "getGlobalContext"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ref $ llvmType "LLVMContext"
                                        , gfunArgs = []
                                        , gfunHSName = "getGlobalContext"
                                        }
             }
       ,Spec { specHeader = irInclude version "Module.h"
             , specNS = ["llvm"]
             , specName = "Module"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(Constructor [(False,normalT $ NamedType ["llvm"] "StringRef" [])
                                        ,(False,normalT $ RefType $ NamedType ["llvm"] "LLVMContext" [])
                                        ],GenHS,"newModule")
                          ,(Destructor False,GenHS,"deleteModule")
                          ,(memberFun { ftReturnType = normalT void
                                      , ftName = "dump"
                                      },GenHS,"moduleDump")
                          ,(memberFun { ftReturnType = normalT $ RefType $ NamedType ["llvm"] "iplist"
                                                       [normalT $ NamedType ["llvm"] "Function" []]
                                      , ftName = "getFunctionList"
                                      },GenHS,"getFunctionList")
                          ,(memberFun { ftReturnType = normalT $ ref $ NamedType ["llvm"] "iplist"
                                                       [normalT $ NamedType ["llvm"] "GlobalVariable" []]
                                      , ftName = "getGlobalList"
                                      },GenHS,"getGlobalList")
                          ]
             }
       ,Spec { specHeader = "llvm/Support/IRReader.h"
             , specNS = []
             , specName = "llvm"
             , specTemplateArgs = []
             , specType = ClassSpec $
                          [(memberFun { ftReturnType = normalT (PtrType $ NamedType ["llvm"] "Module" []) 
                                      , ftName = "ParseIR"
                                      , ftArgs = [(False,normalT (PtrType $ NamedType ["llvm"] "MemoryBuffer" []))
                                                 ,(False,normalT (RefType $ NamedType ["llvm"] "SMDiagnostic" []))
                                                 ,(False,normalT (RefType $ NamedType ["llvm"] "LLVMContext" []))]
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
                                                      ,"ConstantArray"
                                                      ,"ConstantDataSequential"
                                                      ,"ConstantDataArray"
                                                      ,"ConstantDataVector"
                                                      ,"ConstantExpr"
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
                          , let to_tp = normalT $ NamedType ["llvm"] to []
                                from_tp = normalT $ NamedType ["llvm"] from []
                          ]
             }
       ,Spec { specHeader = "llvm/CodeGen/PseudoSourceValue.h"
             , specNS = ["llvm"]
             , specName = "PseudoSourceValue"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Constants.h"
             , specNS = ["llvm"]
             , specName = "ConstantAggregateZero"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instruction.h"
             , specNS = ["llvm"]
             , specName = "Instruction"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "BasicBlock" []
                                      , ftName = "getParent"
                                      },GenHS,"instructionGetParent")
                          ,(memberFun { ftReturnType = constT $ ref $ NamedType ["llvm"] "DebugLoc" []
                                      , ftName = "getDebugLoc"
                                      },GenHS,"instructionGetDebugLoc")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "AtomicCmpXchgInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "AtomicRMWInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "BinaryOperator"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getOpcode"
                                      },GenHS,"binOpGetOpCode_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "CallInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isTailCall"
                                      },GenHS,"callInstIsTailCall")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumArgOperands"
                                      },GenHS,"callInstGetNumArgOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                      , ftName = "getArgOperand"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"callInstGetArgOperand_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                      , ftName = "getCalledValue"
                                      },GenHS,"callInstGetCalledValue")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "CmpInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT int
                                      , ftName = "getPredicate" 
                                      , ftOverloaded = True
                                      },GenHS,"cmpInstGetPredicate_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "FCmpInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "ICmpInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "ExtractElementInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                      , ftName = "getVectorOperand"
                                      },GenHS,"extractElementInstGetVectorOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                      , ftName = "getIndexOperand"
                                      },GenHS,"extractElementInstGetIndexOperand")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "FenceInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "GetElementPtrInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
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
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "InsertElementInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"insertElementInstGetType")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "InsertValueInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "LandingPadInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "PHINode"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumIncomingValues"
                                      },GenHS,"phiNodeGetNumIncomingValues_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                      , ftName = "getIncomingValue"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"phiNodeGetIncomingValue_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "BasicBlock" []
                                      , ftName = "getIncomingBlock"
                                      , ftArgs = [(False,normalT unsigned)]
                                      },GenHS,"phiNodeGetIncomingBlock_")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
             , specName = "ShuffleVectorInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "VectorType" []
                                      , ftName = "getType"
                                      },GenHS,"shuffleVectorInstGetType")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "StoreInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },GenHS,"storeInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },GenHS,"storeInstGetAlignment_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getValueOperand"
                                      },GenHS,"storeInstGetValueOperand")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getPointerOperand"
                                      },GenHS,"storeInstGetPointerOperand")
                          ]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
             , specName = "IndirectBrInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = ["llvm"]
             , specName = "InvokeInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = ["llvm"]
             , specName = "ResumeInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = ["llvm"]
             , specName = "ReturnInst"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getReturnValue"
                                      },GenHS,"returnInstGetReturnValue")]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = ["llvm"]
             , specName = "SwitchInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getCondition"
                                      },GenHS,"switchInstGetCondition")
                          ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                      , ftName = "case_begin"
                                      },GenHS,"switchInstCaseBegin")
                          ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                      , ftName = "case_end"
                                      },GenHS,"switchInstCaseEnd")
                          ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                      , ftName = "case_default"
                                      },GenHS,"switchInstCaseDefault")]
             }
       ,Spec { specHeader = irInclude version "InstrTypes.h"
             , specNS = ["llvm","SwitchInst"]
             , specName = "CaseIt"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                      , ftName = "operator++"
                                      },GenHS,"caseItNext")
                          ,(memberFun { ftReturnType = normalT $ NamedType ["llvm","SwitchInst"] "CaseIt" []
                                      , ftName = "operator--"
                                      },GenHS,"caseItPrev")
                          ,(memberFun { ftReturnType = normalT bool
                                      , ftName = "operator=="
                                      , ftArgs = [(False,constT $ ref $ NamedType ["llvm","SwitchInst"] "CaseIt" [])]
                                      },GenHS,"caseItEq")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "ConstantInt"
                                      , ftName = "getCaseValue"
                                      },GenHS,"caseItGetCaseValue")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "BasicBlock"
                                      , ftName = "getCaseSuccessor"
                                      },GenHS,"caseItGetCaseSuccessor")]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "UnreachableInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "UnaryInstruction"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "AllocaInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "PointerType" []
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
             , specNS = ["llvm"]
             , specName = "CastInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "BitCastInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "FPExtInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "FPToUIInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "FPTruncInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "IntToPtrInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "PtrToIntInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "SExtInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "SIToFPInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "TruncInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "UIToFPInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "ZExtInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "ExtractValueInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "LoadInst"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT bool
                                      , ftName = "isVolatile"
                                      },GenHS,"loadInstIsVolatile")
                          ,(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getAlignment"
                                      },GenHS,"loadInstGetAlignment_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ llvmType "Value"
                                      , ftName = "getPointerOperand"
                                      },GenHS,"loadInstGetPointerOperand")
                          ]
             }
       ,Spec { specHeader = irInclude version "Instructions.h"
             , specNS = ["llvm"]
             , specName = "VAArgInst"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "User.h"
             , specNS = ["llvm"]
             , specName = "User"
             , specTemplateArgs = []
             , specType = ClassSpec
                          [(memberFun { ftReturnType = normalT unsigned
                                      , ftName = "getNumOperands" 
                                      , ftOverloaded = True
                                      },GenHS,"getNumOperands_")
                          ,(memberFun { ftReturnType = normalT $ ptr $ NamedType ["llvm"] "Value" []
                                      , ftName = "getOperand" 
                                      , ftArgs = [(False,normalT unsigned)]
                                      , ftOverloaded = True
                                      },GenHS,"getOperand_")]
             }
       ,Spec { specHeader = irInclude version "Operator.h"
             , specNS = ["llvm"]
             , specName = "Operator"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = irInclude version "Use.h"
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
             , specName = "Pass"
             , specTemplateArgs = []
             , specType = ClassSpec 
                          [(memberFun { ftReturnType = constT $ ptr $ llvmType "PassInfo"
                                      , ftName = "lookupPassInfo"
                                      , ftArgs = [(False,normalT $ llvmType "StringRef")]
                                      , ftStatic = True
                                      },GenHS,"passLookupPassInfo")
                           ]
             }
       ,Spec { specHeader = "llvm/Pass.h"
             , specNS = ["llvm"]
             , specName = "FunctionPass"
             , specTemplateArgs = []
             , specType = ClassSpec []
             }
       ,Spec { specHeader = "llvm/PassSupport.h"
             , specNS = ["llvm"]
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
             , specNS = ["llvm"]
             , specName = "PassManagerBuilder"
             , specTemplateArgs = []
             , specType = ClassSpec
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
                                   },GenHS,"setPassManagerBuilderDisableUnrollLoops")
                          ,(Setter { ftSetVar = "Vectorize"
                                   , ftSetType = normalT bool
                                   },GenHS,"setPassManagerBuilderVectorize")
                          --,(Setter { ftSetVar = "LoopVectorize"
                          --         , ftSetType = normalT bool
                          --         },GenHS,"setPassManagerBuilderLoopVectorize")
                          ]
             }
       ]++
       [Spec { specHeader = "llvm/Transforms/Scalar.h"
             , specNS = ["llvm"]
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
                     ,"createLoopUnrollPass"
                     ,"createLoopUnswitchPass"
                     ,"createMemCpyOptPass"
                     ,"createSCCPPass"
                     ,"createScalarReplAggregatesPass"
                     ,"createSimplifyLibCallsPass"
                     ,"createTailCallEliminationPass"
                     ]
       ]++
       [Spec { specHeader = "llvm/Analysis/Verifier.h"
             , specNS = ["llvm"]
             , specName = "createVerifierPass"
             , specTemplateArgs = []
             , specType = GlobalFunSpec { gfunReturnType = normalT $ ptr $ llvmType "FunctionPass"
                                        , gfunArgs = []
                                        , gfunHSName = "createVerifierPass"
                                        }
             }]