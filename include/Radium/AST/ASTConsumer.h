#ifndef RADIUM_AST_ASTCONSUMER_H
#define RADIUM_AST_ASTCONSUMER_H

namespace Radium {

class Decl;
class ASTContext;

class ASTConsumer {
 public:
  ASTConsumer(ASTContext& context) : context_(context) {}
  virtual ~ASTConsumer() = default;

  ASTConsumer(const ASTConsumer&) = delete;
  void operator=(const ASTConsumer&) = delete;

  auto GetContext() const -> ASTContext& { return context_; }

  virtual void HandleTopLevelDecl(Decl* decl) {}

  virtual void HandleEndOfTranslationUnit() {}

 private:
  ASTContext& context_;
};

}  // namespace Radium

#endif  // RADUM_AST_ASTCONSUMER_H