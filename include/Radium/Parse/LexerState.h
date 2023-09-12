#ifndef RADIUM_PARSE_LEXERSTATE_H
#define RADIUM_PARSE_LEXERSTATE_H

#include "Radium/Basic/SourceLoc.h"
#include "llvm/ADT/Optional.h"

namespace Radium {

class Lexer;

/// 词法分析器的状态。
class LexerState {
  friend class Lexer;

 public:
  LexerState() = default;

  auto isValid() const -> bool { return loc_.isValid(); }

  auto advance(unsigned offset) const -> LexerState {
    assert(isValid());
    return LexerState(loc_.getAdvancedLoc(offset));
  }

 private:
  explicit LexerState(SourceLoc loc) : loc_(loc) {}

  SourceLoc loc_;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_LEXERSTATE_H