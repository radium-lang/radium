#ifndef RADIUM_PARSE_LEXER_H
#define RADIUM_PARSE_LEXER_H

#include <cassert>

#include "Radium/Basic/LangOptions.h"
#include "Radium/Basic/SourceLoc.h"
#include "Radium/Basic/SourceManager.h"
#include "Radium/Parse/LexerState.h"
#include "Radium/Parse/Token.h"
#include "llvm/ADT/SmallVector.h"

namespace Radium {

/// 注释保留模式。
enum class CommentRetentionMode {
  None,
  AttachToNextToken,
  ReturnAsTokens,
};

/// 词法分析器的hashbang模式。
enum class HashbangMode : bool {
  Disallowed,
  Allowed,
};

/// 词法分析器的模式。
enum class LexerMode { Radium, RadiumInterface, RIL };

/// 是否词法分析器应该尝试词法分析一个`/.../`正则表达式字面量。
enum class LexerForwardSlashRegexMode {
  /// 不会词法分析`/.../`正则表达式字面量。
  None,
  /// `/.../`正则表达式字面量将被词法分析，但只有在成功时才会词法分析。
  Tentative,
  /// 对于'/'字符，总是会词法分析`/.../`正则表达式字面量。
  Always
};

/// 词法分析器可能遇到的冲突标记的种类。
enum class ConflictMarkerKind {
  /// 由至少7个"<"s开始的普通或diff3冲突标记，由至少7个"="s或"|"s分隔，
  /// 并由至少7个">"s终止。
  Normal,
  /// A Perforce-style conflict marker, initiated by 4 ">"s,
  /// separated by 4 "="s, and terminated by 4 "<"s.
  /// 由4个">"s开始的Perforce风格的冲突标记，由4个"="s分隔，并且由4个"<"s终止。
  Perforce
};

class Lexer {
  using State = LexerState;

 public:
  struct StringSegment {
    enum : char {
      Literal,
      Expr,
    } kind;
    // loc+length代表字符串字面值内的段，不包括引号。
    SourceLoc loc;
    unsigned length, indent_to_strip, custom_delimiter_len;
    bool is_first_segment, is_last_segment;

    static auto getLiteral(SourceLoc loc, unsigned length,
                           unsigned indent_to_strip,
                           unsigned custom_delimiter_len, bool is_first_segment,
                           bool is_last_segment) -> StringSegment {
      return {Literal,
              loc,
              length,
              indent_to_strip,
              custom_delimiter_len,
              is_first_segment,
              is_last_segment};
    }

    static auto getExpr(SourceLoc loc, unsigned length) -> StringSegment {
      return {Expr, loc, length, 0, 0, false, false};
    }

    auto getEndLoc() -> SourceLoc { return loc.getAdvancedLoc(length); }
  };

  /// 在解析RIL主体时使用此helper类，以通知词法分析器应该启用特定于RIL的词法分析。
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
  enum class NulCharacterKind {
    /// 字符串缓冲区结束符。
    BufferEnd,
    /// 嵌入空字符。
    Embedded,
    /// 代码完成标记。
    CodeCompletion
  };

 private:
  Lexer(const Lexer&) = delete;
  void operator=(const Lexer&) = delete;

  struct PrincipalTag {};

  Lexer(const PrincipalTag&, const LangOptions& lang_opts,
        const SourceManager& src_mgr, unsigned buffer_id, LexerMode lex_mode,
        HashbangMode hashbang_allowed, CommentRetentionMode retain_comments);

  void initialize(unsigned offset, unsigned end_offset);

 public:
  /// 创建一个扫描整个源缓冲区的词法分析器。
  Lexer(const LangOptions& options, const SourceManager& src_mgr,
        unsigned buffer_id, LexerMode lex_mode,
        HashbangMode hashbang_allowed = HashbangMode::Disallowed,
        CommentRetentionMode retain_comments = CommentRetentionMode::None);

  /// 创建一个扫描源缓冲区的子范围的词法分析器。
  Lexer(const LangOptions& options, const SourceManager& src_mgr,
        unsigned buffer_id, LexerMode lex_mode, HashbangMode hashbang_allowed,
        CommentRetentionMode retain_comments, unsigned offset,
        unsigned end_offset);

  /// 创建一个子词法分析器，该子词法分析器从同一缓冲区进行词法分析，
  /// 但扫描缓冲区的子范围。
  Lexer(const Lexer& parent, State begin_state, State end_state);

  // 如果此词法分析器将生成code completion token，则返回true。
  auto isCodeCompletion() const -> bool {
    return code_completion_ptr_ != nullptr;
  }

  /// 判断是否正在访问Radium接口文件。
  auto isRadiumInterface() const -> bool {
    return lex_mode_ == LexerMode::Radium;
  }

  auto isKeepingComments() const -> bool {
    return retain_comments_ == CommentRetentionMode::ReturnAsTokens;
  }

  void lex(Token& result) {
    result = next_token_;
    if (result.isNot(TokenKind::eof)) {
      lexImpl();
    }
  }

  /// 重置lexer的内部状态，以便从给定的偏移量重新开始词法分析。
  void resetToOffset(size_t offset) {
    assert(buffer_start_ + offset <= buffer_end_);
    cur_ptr_ = buffer_start_ + offset;
    lexImpl();
  }

  /// 在当前位置切断lexing，下一个将被lex的token将是一个eof token，
  /// 即使还有未被lex的源码也切断。
  void cutOffLexing() {
    if (lexer_cut_off_point_ == nullptr || lexer_cut_off_point_ >= cur_ptr_) {
      lexer_cut_off_point_ = cur_ptr_;
    }
  }

  /// 如果设置了词法分析器截断点，则返回缓冲区中词法分析器被截断的偏移量。
  /// 如果没有设置截断点，则返回None。
  auto lexingCutOffOffset() const -> llvm::Optional<size_t> {
    if (lexer_cut_off_point_) {
      return lexer_cut_off_point_ - buffer_start_;
    } else {
      return llvm::None;
    }
  }

  auto getBufferID() const -> unsigned { return buffer_id_; }

  // 返回Lex要返回的下一个token，而不实际对其进行lexing。
  auto peekNextToken() const -> const Token& { return next_token_; }

  // 返回给定token位置开头的词法分析器状态。
  // 在恢复状态之后，lexer将返回这个令牌并从此处继续。
  auto getStateForBeginningOfTokenLoc(SourceLoc loc) const -> State;

  auto getStateForBeginningOfToken(const Token& tok) const -> State {
    SourceLoc tok_start = tok.getCommentStart();
    if (tok_start.isInvalid()) {
      tok_start = tok.getLoc();
    }
    auto s = getStateForBeginningOfTokenLoc(tok_start);
    return s;
  }

  auto getStateForEndOfTokenLoc(SourceLoc loc) const -> State {
    return State(getLocForEndOfToken(src_mgr_, loc));
  }

  auto isStateForCurrentBuffer(State state) const -> bool {
    return src_mgr_.findBufferContainingLoc(state.loc_) == getBufferID();
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

  auto getLocForStartOfBuffer() const -> SourceLoc {
    return SourceLoc(llvm::SMLoc::getFromPointer(buffer_start_));
  }

  static auto isIdentifier(llvm::StringRef identifier) -> bool;

  static auto isOperator(llvm::StringRef string) -> bool;

  static auto kindOfIdentifier(llvm::StringRef identifier) -> TokenKind;

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
  static void getStringLiteralSegments(
      const Token& str, SmallVectorImpl<StringSegment>& segments);

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

  void skipToEndOfLine(bool eat_newline);

  // 跳到//注释行末尾。
  void skipSlashSlashComment(bool eat_newline);

  // 跳过一个#! hashbang行。
  void skipHashbang(bool eat_newline);

  void skipSlashStarComment();
  void lexIdentifier();
  void lexDollarIdentifier();
  void lexOperatorIdentifier();
  void lexHexNumber();
  void lexNumber();

  /// 跳过不影响语法的元素并返回在StringRef中跳过的字符。
  /// `is_for_trailing_trivia`是一个布尔标志，用于指示
  /// lexTrivia函数应该解析trailing trivia(true)还是leading trivia(false)。
  auto lexTrivia(bool is_for_trailing_trivia, const char* all_trivia_start)
      -> llvm::StringRef;

  auto lexCharacter(const char*& cur_ptr, bool stop_at_double_quote,
                    bool emit_diagnostics) -> unsigned;
  void lexCharacterLiteral();
  void lexStringLiteral();

  auto tryLexConflictMarker(bool eat_newline) -> bool;

  auto lexUnknown() -> bool;

  /// 用于处理编辑器占位符，占位符的格式是<# ... #>，占位符不允许嵌套。
  void tryLexEditorPlaceholder();

  auto getNulCharacterKind(const char* ptr) const -> NulCharacterKind;

 private:
  /// lang opt
  const LangOptions& lang_opts_;
  /// 源码操作管理。
  const SourceManager& src_mgr_;
  /// 记录buffer id。
  const unsigned buffer_id_;
  /// 指向缓冲区第一个字符的指针，即使扫描buffer的subrange也是第一个字符。
  const char* buffer_start_;
  /// 指向缓冲区最后一个字符的下一个字符的指针。
  /// 因为缓冲区总是以null结束，所以它指向null结束符。
  const char* buffer_end_;
  /// 指向位于buffer_end_之前的人工EOF的指针，在lex subrange的时候有用。
  const char* artificial_eof_ = nullptr;
  /// 如果非空，则指向缓冲区中的'\0'字符，这里需要生成一个code completion
  /// token。
  const char* code_completion_ptr_ = nullptr;
  /// 指向buffer_start_，如果存在，则指向UTF-8 BOM序列的末尾。
  const char* content_start_;
  /// 指向下一个未consume的字符。
  const char* cur_ptr_;
  /// 下一个token。
  Token next_token_;
  /// 词法分析器模式。
  const LexerMode lex_mode_;
  /// 词法分析器的正则解析模式。
  LexerForwardSlashRegexMode forward_slash_regex_mode_ =
      LexerForwardSlashRegexMode::None;
  /// 是否应该跳过hashbang行，跳过为true，否则为false，#!位于文件的开头。
  const bool is_hashbang_allowed_;
  /// 词法分析器的注释保留模式。
  const CommentRetentionMode retain_comments_;
  /// 注释开始的位置。
  const char* comment_start_;
  /// 在RIL文件中对RIL declaration进行词法分析时为true。
  /// 用于支持一些上下文敏感的词法分析。
  bool in_ril_body_ = false;
  /// 用于在检测到嵌套级别过高时提前切断词法分析。
  const char* lexer_cut_off_point_ = nullptr;
};

}  // namespace Radium

#endif  // RADIUM_PARSE_LEXER_H