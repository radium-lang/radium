#ifndef RADIUM_PARSE_LEXER_H
#define RADIUM_PARSE_LEXER_H

#include "Radium/Parse/Token.h"

namespace llvm {
class MemoryBuffer;
class SourceMgr;
}  // namespace llvm

namespace Radium {

class Token;

class Lexer {
 public:
  Lexer(unsigned buffer_id, llvm::SourceMgr& src_mgr);

  void Lex(Token& result);

  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

 private:
  void Warning(const char* loc, const char* message);
  void Error(const char* loc, const char* message);
  void FormToken(Tok::TokenKind kind, const char* tok_start, Token& result);

  void SkipSlashSlashComment();
  void LexIdentifier(Token& result);
  void LexDigit(Token& result);

  llvm::SourceMgr& src_mgr_;
  const llvm::MemoryBuffer* buffer_;
  const char* cur_ptr_;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_LEXER_H