add_llvm_library(LLVMObjCARCOpts
  ObjCARC.cpp
  ObjCARCOpts.cpp
  ObjCARCExpand.cpp
  ObjCARCAPElim.cpp
  ObjCARCAliasAnalysis.cpp
  ARCInstKind.cpp
  ObjCARCContract.cpp
  DependencyAnalysis.cpp
  ProvenanceAnalysis.cpp
  ProvenanceAnalysisEvaluator.cpp
  PtrState.cpp
  )

add_dependencies(LLVMObjCARCOpts intrinsics_gen)
