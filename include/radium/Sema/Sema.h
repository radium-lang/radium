#ifndef RADIUM_SEMA_SEMA_H
#define RADIUM_SEMA_SEMA_H

#include "radium/AST/ASTContext.h"
#include "radium/Sema/SemaExpr.h"
#include "radium/Sema/SemaType.h"

namespace Radium {
class VarDecl;
class Expr;

class Sema {
 public:
  explicit Sema(ASTContext& Context);
  ~Sema() = default;

  Sema(const Sema&) = delete;
  void operator=(const Sema&) = delete;

  VarDecl* ActOnVarDecl(llvm::SMLoc VarLoc, llvm::StringRef Name, Type* Ty,
                        Radium::Expr* Init);

  ASTContext& Context;
  SemaType type;
  SemaExpr expr;
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMA_H