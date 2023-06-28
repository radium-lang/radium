#include "Radium/Sema/SemaExpr.h"

#include "Radium/AST/Expr.h"
#include "Radium/Sema/Sema.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace Radium {

auto SemaExpr::ActOnNumericConstant(llvm::StringRef text, llvm::SMLoc loc)
    -> Expr* {
  return new (sema_.context_)
      IntegerLiteral(text, loc, sema_.context_.int_type_);
}

auto SemaExpr::ActOnParenExpr(llvm::SMLoc lp_loc, Expr* sub_expr,
                              llvm::SMLoc rp_loc) -> Expr* {
  return new (sema_.context_)
      ParenExpr(lp_loc, sub_expr, rp_loc, sema_.context_.int_type_);
}

auto SemaExpr::ActOnBinaryExpr(unsigned kind, Expr* lhs, llvm::SMLoc op_loc,
                               Expr* rhs) -> Expr* {
  return new (sema_.context_) BinaryExpr(static_cast<ExprKind>(kind), lhs,
                                         op_loc, rhs, sema_.context_.int_type_);
}

}  // namespace Radium