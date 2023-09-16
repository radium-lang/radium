#ifndef RADIUM_PARSE_PARSER_H
#define RADIUM_PARSE_PARSER_H

#include "Radium/Parse/Token.h"
#include "llvm/ADT/PointerUnion.h"

namespace llvm {
class SourceMgr;
}  // namespace llvm

namespace Radium {

class Lexer;
class Sema;
class Expr;
class Type;
class Decl;
class ASTContext;
class ASTConsumer;

class Parser {
 public:
  Parser(unsigned buffer_id, ASTConsumer& consumer);
  ~Parser();

  void ParseTranslationUnit();


  Parser(const Parser&);
  void operator=(const Parser&);

  ASTConsumer& consumer_;
  llvm::SourceMgr& src_mgr_;
  Lexer& lexer_;
  Sema& sema_;
  Token token_;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_PARSER_H