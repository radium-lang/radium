#ifndef RADIUM_PARSE_LEXER_H
#define RADIUM_PARSE_LEXER_H

#include "radium/Parse/Token.h"

namespace llvm {
class MemoryBuffer;
class SourceMgr;
}  // namespace llvm

namespace Radium {

class Token;

class Lexer {
 public:
  Lexer(unsigned BufferID, llvm::SourceMgr& SrcMgr);

  void Lex(Token& Result);

  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

 private:
  void Warning(const char* Loc, const char* Message);
  void Error(const char* Loc, const char* Message);
  void FormToken(Tok::TokenKind Kind, const char* TokStart, Token& Result);

  void SkipSlashSlashComment();
  void LexIdentifier(Token& Result);
  void LexDigit(Token& Result);

 private:
  llvm::SourceMgr& SrcMgr;
  const llvm::MemoryBuffer* Buffer;
  const char* CurPtr;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_LEXER_H