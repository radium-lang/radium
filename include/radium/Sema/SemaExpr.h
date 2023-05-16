#ifndef RADIUM_SEMA_SEMAEXPR_H
#define RADIUM_SEMA_SEMAEXPR_H

#include "radium/Sema/SemaBase.h"

namespace Radium {

class Sema;

class SemaExpr : public SemaBase {
 public:
  explicit SemaExpr(Sema& S) : SemaBase(S) {}

  Expr* ActOnNumericConstant(llvm::StringRef Text, llvm::SMLoc Loc);
  Expr* ActOnParenExpr(llvm::SMLoc LPLoc, Expr* SubExpr, llvm::SMLoc RPLoc);
  Expr* ActOnBinaryExpr(unsigned Kind, Expr* LHS, llvm::SMLoc OpLoc, Expr* RHS);
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMAEXPR_H