#include "Radium/Parse/Token.h"

namespace Radium {

auto getTokenText(TokenKind kind) -> llvm::StringRef {
  switch (kind) {
#define RADIUM_KEYWORD(KW) \
  case TokenKind::kw_##KW: \
    return #KW;
#define RADIUM_POUND_KEYWORD(KW) \
  case TokenKind::pound_##KW:    \
    return "#" #KW;
#define RADIUM_PUNCTUATOR(PUN, TEXT) \
  case TokenKind::PUN:               \
    return TEXT;
#include "Radium/Parse/TokenKinds.def"
    default:
      return llvm::StringRef();
  }
}

}  // namespace Radium