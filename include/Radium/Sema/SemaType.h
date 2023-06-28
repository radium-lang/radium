#ifndef RADIUM_SEMA_SEMATYPE_H
#define RADIUM_SEMA_SEMATYPE_H

#include "Radium/Sema/SemaBase.h"
#include "llvm/ADT/PointerUnion.h"

namespace Radium {

class Sema;
class Decl;
class Type;

class SemaType : public SemaBase {
 public:
  explicit SemaType(Sema& sema) : SemaBase(sema) {}

  auto ActOnIntType(llvm::SMLoc loc) -> Type*;
  auto ActOnVoidType(llvm::SMLoc loc) -> Type*;
  auto ActOnTupleType(llvm::SMLoc lp_loc,
                      const llvm::PointerUnion<Type*, Decl*>* elements,
                      unsigned num_elements, llvm::SMLoc rp_loc) -> Type*;
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMATYPE_H