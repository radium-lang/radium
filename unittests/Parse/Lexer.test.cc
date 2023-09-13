#include "Radium/Parse/Lexer.h"

#include <gtest/gtest.h>

#include "Radium/Basic/LangOptions.h"
#include "Radium/Basic/SourceManager.h"

namespace Radium {
namespace {

class LexerTest : public ::testing::Test {
 public:
  LangOptions lang_opts_;
  SourceManager src_mgr_;

  auto tokenizeAndKeepEOF(unsigned buffer_id) -> std::vector<Token> {
    Lexer lexer(lang_opts_, src_mgr_, buffer_id, LexerMode::Radium);
    std::vector<Token> tokens;
    do {
      tokens.emplace_back();
      lexer.lex(tokens.back());
    } while (tokens.back().isNot(TokenKind::eof));
    return tokens;
  }
};

}  // namespace
}  // namespace Radium