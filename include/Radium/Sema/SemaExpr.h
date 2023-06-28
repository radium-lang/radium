#ifndef RADIUM_SEMA_SEMAEXPR_H
#define RADIUM_SEMA_SEMAEXPR_H

#include "Radium/Sema/SemaBase.h"

namespace Radium {

class Sema;

class SemaExpr : public SemaBase {
 public:
  explicit SemaExpr(Sema& sema) : SemaBase(sema) {}

  auto ActOnNumericConstant(llvm::StringRef text, llvm::SMLoc loc) -> Expr*;
  auto ActOnParenExpr(llvm::SMLoc lp_loc, Expr* sub_expr, llvm::SMLoc rp_loc)
      -> Expr*;
  auto ActOnBinaryExpr(unsigned kind, Expr* lhs, llvm::SMLoc op_loc, Expr* rhs)
      -> Expr*;
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMAEXPR_H