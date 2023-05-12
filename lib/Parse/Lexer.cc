#include "radium/Parse/Lexer.h"

#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

using namespace Radium;

Lexer::Lexer(unsigned BufferID, llvm::SourceMgr& SrcMgr) : SrcMgr(SrcMgr) {
  Buffer = SrcMgr.getMemoryBuffer(BufferID);
  CurPtr = Buffer->getBufferStart();
}

void Lexer::Warning(const char* Loc, const char* Message) {
  SrcMgr.PrintMessage(llvm::SMLoc::getFromPointer(Loc),
                      llvm::SourceMgr::DK_Warning, Message);
}

void Lexer::Error(const char* Loc, const char* Message) {
  SrcMgr.PrintMessage(llvm::SMLoc::getFromPointer(Loc),
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

// Lex [a-zA-Z_][a-zA-Z0-9_]*
void Lexer::LexIdentifier(Token& Result) {
  const char* TokStart = CurPtr - 1;
  assert((isalpha(*TokStart) || *TokStart == '_' && "Unexpected start"));

  while (isalnum(*CurPtr) || *CurPtr == '_') {
    ++CurPtr;
  }

  Tok::TokenKind Kind = llvm::StringSwitch<Tok::TokenKind>(
                            llvm::StringRef(TokStart, CurPtr - TokStart))
                            .Case("int", Tok::TokenKind::KW_int)
                            .Case("var", Tok::TokenKind::KW_var)
                            .Default(Tok::TokenKind::Identifier);

  return FormToken(Kind, TokStart, Result);
}

// Lex [0-9]+
void Lexer::LexDigit(Token& Result) {
  const char* TokStart = CurPtr - 1;
  assert(isdigit(*TokStart) && "Unexpected start");

  while (isdigit(*CurPtr)) {
    ++CurPtr;
  }

  return FormToken(Tok::TokenKind::Numeric_Constant, TokStart, Result);
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

      // clang-format off
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
    case '_':
      return LexIdentifier(Result);
        
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return LexDigit(Result);
      // clang-format on
  }
}