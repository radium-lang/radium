#ifndef RADIUM_PARSE_TOKEN_H
#define RADIUM_PARSE_TOKEN_H

#include <cstdint>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace Radium {

namespace Tok {

enum class TokenKind : int8_t {
  Unknown = 0,
  Eof,
  Identifier,
  Numeric_Constant,

#define RADIUM_KEYWORD(Name) KW_##Name,
#define RADIUM_PUNCTUATOR(X, Y) X,
#include "radium/Parse/TokenKind.def"

  NUM_TOKENS,
};

}  // namespace Tok

class Token {
  Tok::TokenKind Kind;

  llvm::StringRef Text;

 public:
  Tok::TokenKind GetKind() const { return Kind; }

  void SetKind(Tok::TokenKind K) { Kind = K; }

  bool Is(Tok::TokenKind K) const { return Kind == K; }
  bool IsNot(Tok::TokenKind K) const { return Kind != K; }

  llvm::SMLoc GetLocation() const {
    return llvm::SMLoc::getFromPointer(Text.begin());
  }

  llvm::StringRef GetText() const { return Text; }
  void SetText(llvm::StringRef T) { Text = T; }

  unsigned GetLength() const { return Text.size(); }

  void SetToken(Tok::TokenKind K, llvm::StringRef T) {
    Kind = K;
    Text = T;
  }
};

}  // namespace Radium

namespace llvm {

template <typename T>
struct IsPodLike;

template <>
struct IsPodLike<Radium::Token> {
  static const bool value = true;
};

}  // namespace llvm

#endif  // RADIUM_PARSE_TOKEN_H