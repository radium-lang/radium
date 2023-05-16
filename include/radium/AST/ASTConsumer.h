#ifndef RADIUM_AST_ASTCONSUMER_H
#define RADIUM_AST_ASTCONSUMER_H

namespace Radium {

class Decl;
class ASTContext;

class ASTConsumer {
  ASTConsumer(const ASTConsumer&) = delete;
  void operator=(const ASTConsumer&) = delete;

 public:
  ASTConsumer(ASTContext& Ctx) : Context(Ctx) {}
  virtual ~ASTConsumer();

  ASTContext& GetContext() const { return Context; }

  virtual void HandleTopLevelDecl(Decl* D) {}

  virtual void HandleEndOfTranslationUnit() {}

 private:
  ASTContext& Context;
};

}

#endif  // RADUM_AST_ASTCONSUMER_H