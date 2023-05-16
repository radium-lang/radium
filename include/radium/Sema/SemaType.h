#ifndef RADIUM_SEMA_SEMATYPE_H
#define RADIUM_SEMA_SEMATYPE_H

#include "radium/Sema/SemaBase.h"
#include "llvm/ADT/PointerUnion.h"

namespace Radium {
class Sema;
class Decl;
class Type;

class SemaType : public SemaBase {
 public:
  explicit SemaType(Sema& S) : SemaBase(S) {}

  Type* ActOnIntType(llvm::SMLoc Loc);
  Type* ActOnVoidType(llvm::SMLoc Loc);
  Type* ActOnTupleType(llvm::SMLoc LPLoc,
                       llvm::PointerUnion<Type*, Decl*> const* Elements,
                       unsigned NumElements, llvm::SMLoc RPLoc);
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMATYPE_H