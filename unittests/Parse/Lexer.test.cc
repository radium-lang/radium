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

  void checkLex(llvm::StringRef source, bool keep_comments = false,
                bool keep_eof = false) {
    unsigned buffer_id = src_mgr_.addMemBufferCopy(source);
    std::vector<Token> tokens;
    tokens = tokenizeAndKeepEOF(buffer_id);
    for (unsigned i = 0, e = tokens.size(); i != e; ++i) {
      std::cout << "[TokenKind: "
                << std::string(getTokenText(tokens[i].getKind()))
                << ", i = " << i << "]\n";
    }
  }
};

TEST_F(LexerTest, TokenizeAndKeepEOF) {
  const char* source =
      "// Blah\n"
      "(/*yo*/)";
  checkLex(source);
}

}  // namespace
}  // namespace Radium