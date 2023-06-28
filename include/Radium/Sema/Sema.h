#ifndef RADIUM_SEMA_SEMA_H
#define RADIUM_SEMA_SEMA_H

#include "Radium/AST/ASTContext.h"
#include "Radium/Sema/SemaExpr.h"
#include "Radium/Sema/SemaType.h"

namespace Radium {

class VarDecl;
class Expr;

class Sema {
 public:
  explicit Sema(ASTContext& context);
  ~Sema() = default;

  Sema(const Sema&) = delete;
  void operator=(const Sema&) = delete;

  auto ActOnVarDecl(llvm::SMLoc var_loc, llvm::StringRef name, Type* type,
                    Radium::Expr* init) -> VarDecl*;

  ASTContext& context_;
  SemaType type_;
  SemaExpr expr_;
};

}  // namespace Radium

#endif  // RADIUM_SEMA_SEMA_H