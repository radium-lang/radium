#ifndef RADIUM_SEMA_SEMABASE_H
#define RADIUM_SEMA_SEMABASE_H

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace Radium {

class Sema;
class Expr;

class SemaBase {
 public:
  explicit SemaBase(Sema& sema) : sema_(sema) {}

  SemaBase(const SemaBase&) = delete;
  void operator=(const SemaBase&) = delete;

  void Note(llvm::SMLoc loc, const char* message);
  void Warning(llvm::SMLoc loc, const char* message);
  void Error(llvm::SMLoc loc, const char* message);

 protected:
  Sema& sema_;
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMABASE_H