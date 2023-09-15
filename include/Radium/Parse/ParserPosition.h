#ifndef RADIUM_PARSE_PARSERPOSITION_H
#define RADIUM_PARSE_PARSERPOSITION_H

#include "Radium/Basic/SourceLoc.h"
#include "Radium/Parse/LexerState.h"

namespace Radium {

class ParserPosition {
 public:
  ParserPosition() = default;

  auto isValid() const -> bool { return state_.isValid(); }

 private:
  ParserPosition(LexerState state, SourceLoc previous_loc)
      : state_(state), previous_loc_(previous_loc) {}

  LexerState state_;
  SourceLoc previous_loc_;
  friend class Parser;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_PARSERPOSITION_H