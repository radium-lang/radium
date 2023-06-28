#include "Radium/Sema/SemaType.h"

#include "Radium/AST/Type.h"
#include "Radium/Sema/Sema.h"
#include "llvm/Support/SMLoc.h"

namespace Radium {

auto SemaType::ActOnIntType(llvm::SMLoc loc) -> Type* {
  return sema_.context_.int_type_;
}

auto SemaType::ActOnVoidType(llvm::SMLoc loc) -> Type* {
  return sema_.context_.void_type_;
}

auto SemaType::ActOnTupleType(llvm::SMLoc lp_loc,
                              const llvm::PointerUnion<Type*, Decl*>* elements,
                              unsigned num_elements, llvm::SMLoc rp_loc)
    -> Type* {
  return sema_.context_.void_type_;
}

}  // namespace Radium