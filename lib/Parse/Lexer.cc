#include "radium/Parse/Lexer.h"

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

using namespace Radium;

Lexer::Lexer(unsigned BufferID, llvm::SourceMgr& SrcMgr) : SrcMgr(SrcMgr) {
  Buffer = SrcMgr.getMemoryBuffer(BufferID);
  CurPtr = Buffer->getBufferStart();
}

void Lexer::Warning(const char* Loc, const char* Message) {
  SrcMgr.PrintMessage(llvm::errs(), llvm::SMLoc::getFromPointer(Loc),
                      llvm::SourceMgr::DK_Warning, Message);
}

void Lexer::Error(const char* Loc, const char* Message) {
  SrcMgr.PrintMessage(llvm::errs(), llvm::SMLoc::getFromPointer(Loc),
                      llvm::SourceMgr::DK_Error, Message);
}

void Lexer::FormToken(Tok::TokenKind Kind, const char* TokStart,
                      Token& Result) {
  Result.SetToken(Kind, llvm::StringRef(TokStart, CurPtr - TokStart));
}

void Lexer::SkipSlashSlashComment() {
  assert(CurPtr[0] == '/' && CurPtr[-1] == '/' && "Not a // comment");
  while (true) {
    switch (*CurPtr++) {
      case '\n':
      case '\r':
        return;
      case 0:
        // skip the null character if it in the buffer
        if (CurPtr - 1 != Buffer->getBufferEnd()) {
          Warning(CurPtr - 1, "null character embedded in file");
          break;
        }

        --CurPtr;
        Warning(CurPtr - 1, "no newline at end of // comment");
        return;
      default:
        break;
    }
  }
}

void Lexer::Lex(Token& Result) {
  assert(CurPtr >= Buffer->getBufferStart() &&
         CurPtr <= Buffer->getBufferEnd() &&
         "Current char pointer out of range!");

restart:
  const char* TokStart = CurPtr;

  switch (*CurPtr++) {
    default:
      Error(CurPtr - 1, "Invalid character");
      return FormToken(Tok::TokenKind::Unknown, TokStart, Result);

    case ' ':
    case '\t':
    case '\n':
    case '\r':
      goto restart;
    case 0:
      // skip the null character if it in the buffer
      if (CurPtr - 1 != Buffer->getBufferEnd()) {
        Warning(CurPtr - 1, "null character embedded in file");
        goto restart;
      }

      return FormToken(Tok::TokenKind::Eof, TokStart, Result);

    case ',':
      return FormToken(Tok::TokenKind::Comma, TokStart, Result);
    case ':':
      return FormToken(Tok::TokenKind::Colon, TokStart, Result);
    case ';':
      return FormToken(Tok::TokenKind::Semi, TokStart, Result);
    case '=':
      return FormToken(Tok::TokenKind::Equal, TokStart, Result);
    case '+':
      return FormToken(Tok::TokenKind::Plus, TokStart, Result);
    case '-':
      return FormToken(Tok::TokenKind::Minus, TokStart, Result);
    case '*':
      return FormToken(Tok::TokenKind::Star, TokStart, Result);
    case '/':
      if (CurPtr[-1] == '/') {
        SkipSlashSlashComment();
        goto restart;
      }
      return FormToken(Tok::TokenKind::Slash, TokStart, Result);
  }
}