#ifndef RADIUM_PARSE_TOKEN_H
#define RADIUM_PARSE_TOKEN_H

#include <cstdint>

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"

namespace Radium {

namespace Tok {

enum class TokenKind : uint8_t {
  Unknown = 0,
  Eof,
  Identifier,
  Numeric_Constant,

#define RADIUM_KEYWORD(Name) KW_##Name,
#define RADIUM_PUNCTUATOR(X, Y) X,
#include "Radium/Parse/TokenKind.def"

  NUM_TOKENS,
};

}  // namespace Tok

class Token {
 public:
  auto GetKind() const -> Tok::TokenKind { return kind_; }

  void Setkind(Tok::TokenKind k) { kind_ = k; }

  auto Is(Tok::TokenKind k) const -> bool { return kind_ == k; }
  auto IsNot(Tok::TokenKind k) const -> bool { return kind_ != k; }

  auto GetLocation() const -> llvm::SMLoc {
    return llvm::SMLoc::getFromPointer(text_.begin());
  }

  auto GetText() const -> llvm::StringRef { return text_; }
  void SetText(llvm::StringRef t) { text_ = t; }

  auto GetLength() const -> unsigned { return text_.size(); }

  void SetToken(Tok::TokenKind k, llvm::StringRef t) {
    kind_ = k;
    text_ = t;
  }

 private:
  Tok::TokenKind kind_;

  llvm::StringRef text_;
};

}  // namespace Radium

namespace llvm {

template <typename T>
struct IsPodLike;

template <>
struct IsPodLike<Radium::Token> {
  static const bool Value = true;
};

}  // namespace llvm

#endif  // RADIUM_PARSE_TOKEN_H