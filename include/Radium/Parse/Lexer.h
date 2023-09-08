#ifndef RADIUM_PARSE_LEXER_H
#define RADIUM_PARSE_LEXER_H

#include "Radium/Basic/SourceLoc.h"
#include "Radium/Basic/SourceManager.h"
#include "Radium/Parse/Token.h"
#include "llvm/ADT/SmallVector.h"

namespace Radium {

class Lexer {
 public:
  class State {
    friend class Lexer;

   public:
    State() = default;

    auto advance(unsigned offset) const -> State {
      assert(isValid());
      return State(loc_.getAdvancedLoc(offset));
    }

   private:
    explicit State(SourceLoc loc) : loc_(loc) {}

    auto isValid() const -> bool { return loc_.isValid(); }

    SourceLoc loc_;
  };

  struct StringSegment {
    enum : char {
      Literal,
      Expr,
    } kind;
    SourceLoc loc;
    unsigned length;

    static auto getLiteral(SourceLoc loc, unsigned length) -> StringSegment {
      return {Literal, loc, length};
    }

    static auto getExpr(SourceLoc loc, unsigned length) -> StringSegment {
      return {Expr, loc, length};
    }
  };

  // 在解析RIL主体时使用此helper类，以通知词法分析器应该启用特定于RIL的词法分析。
  struct RILBodyRAII {
    Lexer& lexer;

    RILBodyRAII(Lexer& l) : lexer(l) {
      assert(!lexer.in_ril_body_ && "Already in a fil body?");
      lexer.in_ril_body_ = true;
    }

    ~RILBodyRAII() {
      assert(lexer.in_ril_body_ && "Left ril body already?");
      lexer.in_ril_body_ = false;
    }

    RILBodyRAII(const RILBodyRAII&) = delete;
    void operator=(const RILBodyRAII&) = delete;
  };

 private:
  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

  Lexer(const SourceManager& src_mgr, bool in_ril_mode, unsigned buffer_id,
        bool keep_comments);

  void primeLexer();

  void initSubLexer(Lexer& parent, State begin_state, State end_state);

 public:
  // 创建一个扫描整个源缓冲区的普通词法分析器。
  Lexer(const SourceManager& src_mgr, unsigned buffer_id, bool in_ril_mode,
        bool keep_comments = false)
      : Lexer(src_mgr, in_ril_mode, buffer_id, keep_comments) {
    primeLexer();
  }

  // 创建一个扫描源缓冲区的子范围的词法分析器。
  Lexer(const SourceManager& src_mgr, unsigned buffer_id, bool in_ril_mode,
        bool keep_comments, unsigned offset, unsigned end_offset)
      : Lexer(src_mgr, in_ril_mode, buffer_id, keep_comments) {
    assert(offset <= end_offset && "Invalid range");
    initSubLexer(*this, State(getLocForStartOfBuffer().getAdvancedLoc(offset)),
                 State(getLocForStartOfBuffer().getAdvancedLoc(end_offset)));
  }

  Lexer(Lexer& parent, State begin_state, State end_state)
      : Lexer(parent.src_mgr_, parent.in_ril_mode_, parent.buffer_id_,
              parent.isKeepingComments()) {
    initSubLexer(parent, begin_state, end_state);
  }

  auto isKeepingComments() const -> bool { return keep_comments_; }

  // 如果此词法分析器将生成code completion token.，则返回true。
  auto isCodeCompletion() const -> bool {
    return code_completion_ptr_ != nullptr;
  }

  void lex(Token& result) {
    result = next_token_;
    if (result.isNot(TokenKind::eof)) {
      lexImpl();
    }
  }

  // 返回Lex要返回的下一个token，而不实际对其进行lexing。
  auto peekNextToken() const -> const Token& { return next_token_; }

  // 返回给定token位置开头的词法分析器状态。
  // 在恢复状态之后，lexer将返回这个令牌并从此处继续。
  auto getStateForBeginningOfTokenLoc(SourceLoc loc) const -> State;

  auto getStateForBeginningOfToken(const Token& tok) const -> State {
    return getStateForBeginningOfTokenLoc(tok.getLoc());
  }

  auto getStateForEndOfTokenLoc(SourceLoc loc) const -> State {
    return State(getLocForEndOfToken(src_mgr_, loc));
  }

  // 将词法分析器状态恢复到给定的状态，该状态可以位于当前位置之前或之后。
  void restoreState(State state) {
    assert(state.isValid());
    cur_ptr_ = getBufferPtrForSourceLoc(state.loc_);
    lexImpl();
  }

  // 将词法分析器状态恢复到位于当前位置之前的给定状态。
  void backtrackToState(State state) {
    assert(getBufferPtrForSourceLoc(state.loc_) <= cur_ptr_ &&
           "can't backtrack forward");
    restoreState(state);
  }

  // 检索指向loc所引用的token末尾后面的源位置。
  static auto getLocForEndOfToken(const SourceManager& sm, SourceLoc loc)
      -> SourceLoc;

  // 返回给定缓冲区中的偏移量所指向的token的起始位置。
  static auto getLocForStartOfToken(SourceManager& sm, unsigned buffer_id,
                                    unsigned offset) -> SourceLoc;

  static auto isIdentifier(StringRef identifier) -> bool;

  auto getLocForStartOfBuffer() const -> SourceLoc {
    return SourceLoc(llvm::SMLoc::getFromPointer(buffer_start_));
  }

  // 计算实际字符串字面量应该编码的字节数。
  static auto getEncodedStringSegment(StringRef str,
                                      SmallVectorImpl<char>& buffer)
      -> StringRef;
  auto getEncodedStringSegment(StringSegment segment,
                               SmallVectorImpl<char>& buffer) const
      -> StringRef {
    return getEncodedStringSegment(
        StringRef(getBufferPtrForSourceLoc(segment.loc), segment.length),
        buffer);
  }

  // 给定一个字符串字面量，将其分成可能被篡改的字符串的string/expr段。
  static void getStringLiteralSegments(const Token& str,
                                       SmallVectorImpl<StringSegment>& segments);

  // 返回指定字符字面量的UTF32码点。
  auto getEncodedCharacterLiteral(const Token& tok) -> uint32_t;

  static auto getSourceLoc(const char* loc) -> SourceLoc {
    return SourceLoc(llvm::SMLoc::getFromPointer(loc));
  }

  // 获取从给定位置开始的token。
  auto getTokenAt(SourceLoc loc) -> Token;

 private:
  /// 对于当前缓冲区中的源位置，返回相应的指针。
  auto getBufferPtrForSourceLoc(SourceLoc loc) const -> const char* {
    return buffer_start_ + src_mgr_.getLocOffsetInBuffer(loc, buffer_id_);
  }

  void lexImpl();

  void formToken(TokenKind kind, const char* tok_start);

  void skipToEndOfLine();

  // 跳到//注释行末尾。
  void skipSlashSlashComment();

  // 跳过一个#! hashbang行。
  void skipHashbang();

  void skipSlashStarComment();
  void lexIdentifier();
  void lexDollarIdentifier();
  void lexOperatorIdentifier();
  void lexHexNumber();
  void lexNumber();

  auto lexCharacter(const char*& cur_ptr, bool stop_at_double_quote,
                    bool emit_diagnostics) -> unsigned;
  void lexCharacterLiteral();
  void lexStringLiteral();

 private:
  const SourceManager& src_mgr_;
  const unsigned buffer_id_;
  // 指向缓冲区第一个字符的指针，即使扫描buffer的subrange也是第一个字符。
  const char* buffer_start_;
  // 指向缓冲区最后一个字符的下一个字符的指针。
  // 因为缓冲区总是以null结束，所以它指向null结束符。
  const char* buffer_end_;
  // 指向位于buffer_end_之前的人工EOF的指针，在lex subrange的时候有用。
  const char* artificial_eof_ = nullptr;
  // 如果非空，则指向缓冲区中的'\0'字符，这里需要生成一个code completion token。
  const char* code_completion_ptr_ = nullptr;
  // 指向下一个未consume的字符。
  const char* cur_ptr_;
  // 下一个token。
  Token next_token_;
  // lex .ril文件而不是.radium的时候，启用ril模式。
  const bool in_ril_mode_;
  // 设置为true时返回comment token，而不是跳过。
  const bool keep_comments_;
  // 在RIL文件中对RIL declaration进行词法分析时为true。
  // 用于支持一些上下文敏感的词法分析。
  bool in_ril_body_ = false;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_LEXER_H