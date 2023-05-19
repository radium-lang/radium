#ifndef RADIUM_SEMA_SEMABASE_H
#define RADIUM_SEMA_SEMABASE_H

#include "llvm/Support/SMLoc.h"
#include "llvm/ADT/StringRef.h"

namespace Radium {

class Sema;
class Expr;

class SemaBase {
  SemaBase(const SemaBase&);
  void operator=(const SemaBase&);

 public:
  explicit SemaBase(Sema& S) : S(S) {}

  void Note(llvm::SMLoc Loc, const char* Message);
  void Warning(llvm::SMLoc Loc, const char* Message);
  void Error(llvm::SMLoc Loc, const char* Message);

 protected:
  Sema& S;
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMABASE_H