#include "radium/Sema/Sema.h"

#include "radium/AST/Decl.h"

using namespace Radium;

Sema::Sema(ASTContext& context) : Context(context), type(*this), expr(*this) {}

VarDecl* Sema::ActOnVarDecl(llvm::SMLoc VarLoc, llvm::StringRef Name, Type* Ty,
                            Radium::Expr* Init) {
  if (Ty == 0 && Init == 0) {
    expr.Error(VarLoc,
               "var declaration must specify a type if no "
               "initializer is specified");
    return 0;
  }
  return new (Context) VarDecl(VarLoc, Name, Ty, Init);
}