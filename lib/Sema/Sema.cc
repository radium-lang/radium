#include "Radium/Sema/Sema.h"

#include "Radium/AST/Decl.h"

namespace Radium {

Sema::Sema(ASTContext& context)
    : context_(context), type_(*this), expr_(*this) {}

auto Sema::ActOnVarDecl(llvm::SMLoc var_loc, llvm::StringRef name, Type* type,
                        Radium::Expr* init) -> VarDecl* {
  // if (Ty == nullptr && Init == nullptr) {
  //   expr.Error(VarLoc,
  //              "var declaration must specify a type if no "
  //              "initializer is specified");
  //   return nullptr;
  // }
  // return new (Context) VarDecl(VarLoc, Name, Ty, Init);

  return nullptr;
}

}  // namespace Radium