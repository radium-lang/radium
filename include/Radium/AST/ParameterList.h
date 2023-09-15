#ifndef RADIUM_AST_PARAMETERLIST_H
#define RADIUM_AST_PARAMETERLIST_H

#include "Radium/AST/ASTContext.h"
#include "Radium/AST/Decl.h"
#include "Radium/Basic/SourceLoc.h"

namespace Radium {

class ParameterList final {
 private:
  ParameterList(SourceLoc l_paren_loc, size_t num_parameters,
                SourceLoc r_paren_loc)
      : l_paren_loc_(l_paren_loc),
        r_paren_loc_(r_paren_loc),
        num_parameters_(num_parameters) {}

  void operator=(const ParameterList&) = delete;

 public:
  static auto create(const ASTContext& ctx, SourceLoc l_paren_loc,
                     llvm::ArrayRef<ParamDecl*> params, SourceLoc r_paren_loc)
      -> ParameterList*;

  // static auto create(const ASTContext& ctx, llvm::ArrayRef<ParamDecl*> params)
  //     -> ParameterList* {
  //   return create(ctx, SourceLoc(), params, SourceLoc());
  // }

 private:
  SourceLoc l_paren_loc_, r_paren_loc_;
  size_t num_parameters_;
};

}  // namespace Radium

#endif  // RADIUM_AST_PARAMETERLIST_H