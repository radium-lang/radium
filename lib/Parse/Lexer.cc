#include "Radium/Parse/Lexer.h"

#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

namespace Radium {

Lexer::Lexer(unsigned buffer_id, llvm::SourceMgr& src_mgr) : src_mgr_(src_mgr) {
  buffer_ = src_mgr_.getMemoryBuffer(buffer_id);
  cur_ptr_ = buffer_->getBufferStart();
}

void Lexer::Warning(const char* loc, const char* message) {
  src_mgr_.PrintMessage(llvm::SMLoc::getFromPointer(loc),
                        llvm::SourceMgr::DK_Warning, message);
}

void Lexer::Error(const char* loc, const char* message) {
  src_mgr_.PrintMessage(llvm::SMLoc::getFromPointer(loc),
                        llvm::SourceMgr::DK_Error, message);
}

void Lexer::FormToken(Tok::TokenKind kind, const char* tok_start,
                      Token& result) {
  result.SetToken(kind, llvm::StringRef(tok_start, cur_ptr_ - tok_start));
}

void Lexer::SkipSlashSlashComment() {
  assert(cur_ptr_[0] == '/' && cur_ptr_[-1] == '/' && "Not a // comment");
  while (true) {
    switch (*cur_ptr_++) {
      case '\n':
      case '\r':
        return;
      case 0:
        // skip the null character if it in the buffer
        if (cur_ptr_ - 1 != buffer_->getBufferEnd()) {
          Warning(cur_ptr_ - 1, "null character embedded in file");
          break;
        }

        --cur_ptr_;
        Warning(cur_ptr_ - 1, "no newline at end of // comment");
        return;
      default:
        break;
    }
  }
}

// Lex [a-zA-Z_][a-zA-Z0-9_]*
void Lexer::LexIdentifier(Token& result) {
  const char* tok_start = cur_ptr_ - 1;
  assert((isalpha(*tok_start) || *tok_start == '_' && "Unexpected start"));

  while (isalnum(*cur_ptr_) || *cur_ptr_ == '_') {
    ++cur_ptr_;
  }

  Tok::TokenKind kind = llvm::StringSwitch<Tok::TokenKind>(
                            llvm::StringRef(tok_start, cur_ptr_ - tok_start))
                            .Case("void", Tok::TokenKind::KW_void)
                            .Case("int", Tok::TokenKind::KW_int)
                            .Case("var", Tok::TokenKind::KW_var)
                            .Default(Tok::TokenKind::Identifier);

  return FormToken(kind, tok_start, result);
}

// Lex [0-9]+
void Lexer::LexDigit(Token& result) {
  const char* tok_start = cur_ptr_ - 1;
  assert(isdigit(*tok_start) && "Unexpected start");

  while (isdigit(*cur_ptr_)) {
    ++cur_ptr_;
  }

  return FormToken(Tok::TokenKind::Numeric_Constant, tok_start, result);
}

void Lexer::Lex(Token& result) {
  assert(cur_ptr_ >= buffer_->getBufferStart() &&
         cur_ptr_ <= buffer_->getBufferEnd() &&
         "Current char pointer out of range!");

restart:
  const char* tok_start = cur_ptr_;

  switch (*cur_ptr_++) {
    default:
      Error(cur_ptr_ - 1, "Invalid character");
      return FormToken(Tok::TokenKind::Unknown, tok_start, result);

    case ' ':
    case '\t':
    case '\n':
    case '\r':
      goto restart;
    case 0:
      // skip the null character if it in the buffer
      if (cur_ptr_ - 1 != buffer_->getBufferEnd()) {
        Warning(cur_ptr_ - 1, "null character embedded in file");
        goto restart;
      }

      return FormToken(Tok::TokenKind::Eof, tok_start, result);

    case ',':
      return FormToken(Tok::TokenKind::Comma, tok_start, result);
    case ':':
      return FormToken(Tok::TokenKind::Colon, tok_start, result);
    case ';':
      return FormToken(Tok::TokenKind::Semi, tok_start, result);
    case '=':
      return FormToken(Tok::TokenKind::Equal, tok_start, result);
    case '+':
      return FormToken(Tok::TokenKind::Plus, tok_start, result);
    case '-':
      if (cur_ptr_[0] == '>') {
        ++cur_ptr_;
        return FormToken(Tok::TokenKind::Arrow, tok_start, result);
      }
      return FormToken(Tok::TokenKind::Minus, tok_start, result);
    case '*':
      return FormToken(Tok::TokenKind::Star, tok_start, result);
    case '/':
      if (cur_ptr_[0] == '/') {
        SkipSlashSlashComment();
        goto restart;
      }
      return FormToken(Tok::TokenKind::Slash, tok_start, result);

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
      return LexIdentifier(result);
        
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return LexDigit(result);
      // clang-format on
  }
}

}  // namespace Radium