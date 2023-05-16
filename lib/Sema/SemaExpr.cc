#include "radium/Sema/SemaExpr.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include "radium/AST/Expr.h"
#include "radium/Sema/Sema.h"

using namespace Radium;

Expr* SemaExpr::ActOnNumericConstant(llvm::StringRef Text, llvm::SMLoc Loc) {
  return new (S.Context) IntegerLiteral(Text, Loc, S.Context.IntType);
}

Expr* SemaExpr::ActOnParenExpr(llvm::SMLoc LPLoc, Expr* SubExpr,
                               llvm::SMLoc RPLoc) {
  return new (S.Context) ParenExpr(LPLoc, SubExpr, RPLoc, S.Context.IntType);
}

Expr* SemaExpr::ActOnBinaryExpr(unsigned Kind, Expr* LHS, llvm::SMLoc OpLoc,
                                Expr* RHS) {
  return new (S.Context)
      BinaryExpr((ExprKind)Kind, LHS, OpLoc, RHS, S.Context.IntType);
}