#include "radium/Sema/SemaType.h"
#include "radium/Sema/Sema.h"
#include "radium/AST/Type.h"
#include "llvm/Support/SMLoc.h"

using namespace Radium;

Type* SemaType::ActOnIntType(llvm::SMLoc Loc) {
  return S.Context.IntType;
}

Type* SemaType::ActOnVoidType(llvm::SMLoc Loc) {
  return S.Context.VoidType;
}

Type* SemaType::ActOnTupleType(llvm::SMLoc LPLoc,
                               llvm::PointerUnion<Type*, Decl*> const* Elements,
                               unsigned NumElements, llvm::SMLoc RPLoc) {
  return S.Context.VoidType;
}