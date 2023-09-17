#ifndef RADIUM_AST_ASTCONTEXT_H
#define RADIUM_AST_ASTCONTEXT_H

#include "Radium/Basic/LangOptions.h"
#include "Radium/Basic/SourceManager.h"

namespace Radium {

class ASTContext final {
 private:
  ASTContext(LangOptions& lang_opts, SourceManager& sm);

 public:
  struct Implementation;

  auto getImpl() const -> Implementation&;

  static auto get(LangOptions& lang_opts, SourceManager& sm) -> ASTContext*;

  ~ASTContext();

  ASTContext(const ASTContext&) = delete;
  void operator=(const ASTContext&) = delete;

 public:
  const LangOptions& lang_opts;

  SourceManager& src_mgr;
};

}  // namespace Radium

#endif  // RADIUM_AST_ASTCONTEXT_H