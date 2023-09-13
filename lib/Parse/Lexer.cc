#include "Radium/Parse/Lexer.h"

#include "Radium/AST/Identifier.h"
#include "Radium/Basic/Fallthrough.h"
#include "Radium/Basic/LangOptions.h"
#include "Radium/Basic/SourceManager.h"
#include "clang/Basic/CharInfo.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/MemoryBuffer.h"

namespace Radium {

//===----------------------------------------------------------------------===//
// UTF8 Validation/Encoding/Decoding helper functions
//===----------------------------------------------------------------------===//
static auto EncodeToUTF8(unsigned char_value, SmallVectorImpl<char>& result)
    -> bool {
  // Number of bits in the value, ignoring leading zeros.
  unsigned num_bits = 32 - llvm::countLeadingZeros(char_value);

  // Handle the leading byte, based on the number of bits in the value.
  unsigned num_trailing_bytes;
  if (num_bits <= 5 + 6) {
    // Encoding is 0x110aaaaa 10bbbbbb
    result.push_back(static_cast<char>(0xC0 | (char_value >> 6)));
    num_trailing_bytes = 1;
  } else if (num_bits <= 4 + 6 + 6) {
    // Encoding is 0x1110aaaa 10bbbbbb 10cccccc
    result.push_back(static_cast<char>(0xE0 | (char_value >> (6 + 6))));
    num_trailing_bytes = 2;

    // UTF-16 surrogate pair values are not valid code points.
    if (char_value >= 0xD800 && char_value <= 0xDFFF) {
      return true;
    }
    // U+FDD0...U+FDEF are also reserved
    if (char_value >= 0xFDD0 && char_value <= 0xFDEF) {
      return true;
    }
  } else if (num_bits <= 3 + 6 + 6 + 6) {
    // Encoding is 0x11110aaa 10bbbbbb 10cccccc 10dddddd
    result.push_back(static_cast<char>(0xF0 | (char_value >> (6 + 6 + 6))));
    num_trailing_bytes = 3;
    // Reject over-large code points.  These cannot be encoded as UTF-16
    // surrogate pairs, so UTF-32 doesn't allow them.
    if (char_value > 0x10FFFF) {
      return true;
    }
  } else {
    return true;  // UTF8 can encode these, but they aren't valid code points.
  }

  // Emit all of the trailing bytes.
  while (num_trailing_bytes--) {
    result.push_back(static_cast<char>(
        0x80 | (0x3F & (char_value >> (num_trailing_bytes * 6)))));
  }
  return false;
}

/// CLO8 - Return the number of leading ones in the specified 8-bit value.
static auto CLO8(unsigned char c) -> unsigned {
  return llvm::countLeadingOnes(static_cast<uint32_t>(c) << 24);
}

/// isStartOfUTF8Character - Return true if this isn't a UTF8 continuation
/// character, which will be of the form 0b10XXXXXX
static auto isStartOfUTF8Character(unsigned char c) -> bool {
  // RFC 2279: The octet values FE and FF never appear.
  // RFC 3629: The octet values C0, C1, F5 to FF never appear.
  return c <= 0x80 || (c >= 0xC2 && c < 0xF5);
}

/// validateUTF8CharacterAndAdvance - Given a pointer to the starting byte of a
/// UTF8 character, validate it and advance the lexer past it.  This returns the
/// encoded character or ~0U if the encoding is invalid.
auto validateUTF8CharacterAndAdvance(const char*& ptr, const char* end)
    -> uint32_t {
  if (ptr >= end) {
    return ~0U;
  }

  unsigned char cur_byte = *ptr++;
  if (cur_byte < 0x80) {
    return cur_byte;
  }

  // Read the number of high bits set, which indicates the number of bytes in
  // the character.
  unsigned encoded_bytes = CLO8(cur_byte);

  // If this is 0b10XXXXXX, then it is a continuation character.
  if (encoded_bytes == 1 || !isStartOfUTF8Character(cur_byte)) {
    // Skip until we get the start of another character.  This is guaranteed to
    // at least stop at the nul at the end of the buffer.
    while (ptr < end && !isStartOfUTF8Character(*ptr)) {
      ++ptr;
    }
    return ~0U;
  }

  // Drop the high bits indicating the # bytes of the result.
  unsigned char_value =
      static_cast<unsigned char>(cur_byte << encoded_bytes) >> encoded_bytes;

  // Read and validate the continuation bytes.
  for (unsigned i = 1; i != encoded_bytes; ++i) {
    if (ptr >= end) {
      return ~0U;
    }
    cur_byte = *ptr;
    // If the high bit isn't set or the second bit isn't clear, then this is not
    // a continuation byte!
    if (cur_byte < 0x80 || cur_byte >= 0xC0) {
      return ~0U;
    }

    // Accumulate our result.
    char_value <<= 6;
    char_value |= cur_byte & 0x3F;
    ++ptr;
  }

  // UTF-16 surrogate pair values are not valid code points.
  if (char_value >= 0xD800 && char_value <= 0xDFFF) {
    return ~0U;
  }

  // If we got here, we read the appropriate number of accumulated bytes.
  // Verify that the encoding was actually minimal.
  // Number of bits in the value, ignoring leading zeros.
  unsigned num_bits = 32 - llvm::countLeadingZeros(char_value);

  if (num_bits <= 5 + 6) {
    return encoded_bytes == 2 ? char_value : ~0U;
  }
  if (num_bits <= 4 + 6 + 6) {
    return encoded_bytes == 3 ? char_value : ~0U;
  }
  return encoded_bytes == 4 ? char_value : ~0U;
}

//===----------------------------------------------------------------------===//
// advance helper
//===----------------------------------------------------------------------===//

/// 从cur_ptr的字符串中一直查找到行尾或找到其中的某些特殊字符（特指换行符）。
static auto advanceToEndOfLine(const char*& cur_ptr, const char* buffer_end,
                               const char* code_completion_ptr = nullptr)
    -> bool {
  while (true) {
    switch (*cur_ptr++) {
      case '\n':
      case '\r':
        --cur_ptr;
        return true;
      default:
        // TODO: diag
        break;
      case 0:  // NUL 字符。
        if (cur_ptr - 1 != buffer_end) {
          // TODO: diag
          continue;
        }
        --cur_ptr;
        // 如果它是缓冲区的最后一个字符，那么文件的最后一行没有换行符，则返回false。
        return false;
    }
  }
}

static auto isValidIdentifierContinuationCodePoint(uint32_t c) -> bool {
  if (c < 0x80) {
    return clang::isAsciiIdentifierContinue(c, /*dollar*/ true);
  }

  // N1518: Recommendations for extended identifier characters for C and C++
  // Proposed Annex X.1: Ranges of characters allowed
  return c == 0x00A8 || c == 0x00AA || c == 0x00AD || c == 0x00AF ||
         (c >= 0x00B2 && c <= 0x00B5) || (c >= 0x00B7 && c <= 0x00BA) ||
         (c >= 0x00BC && c <= 0x00BE) || (c >= 0x00C0 && c <= 0x00D6) ||
         (c >= 0x00D8 && c <= 0x00F6) || (c >= 0x00F8 && c <= 0x00FF)

         || (c >= 0x0100 && c <= 0x167F) || (c >= 0x1681 && c <= 0x180D) ||
         (c >= 0x180F && c <= 0x1FFF)

         || (c >= 0x200B && c <= 0x200D) || (c >= 0x202A && c <= 0x202E) ||
         (c >= 0x203F && c <= 0x2040) || c == 0x2054 ||
         (c >= 0x2060 && c <= 0x206F)

         || (c >= 0x2070 && c <= 0x218F) || (c >= 0x2460 && c <= 0x24FF) ||
         (c >= 0x2776 && c <= 0x2793) || (c >= 0x2C00 && c <= 0x2DFF) ||
         (c >= 0x2E80 && c <= 0x2FFF)

         || (c >= 0x3004 && c <= 0x3007) || (c >= 0x3021 && c <= 0x302F) ||
         (c >= 0x3031 && c <= 0x303F)

         || (c >= 0x3040 && c <= 0xD7FF)

         || (c >= 0xF900 && c <= 0xFD3D) || (c >= 0xFD40 && c <= 0xFDCF) ||
         (c >= 0xFDF0 && c <= 0xFE44) || (c >= 0xFE47 && c <= 0xFFF8)

         || (c >= 0x10000 && c <= 0x1FFFD) || (c >= 0x20000 && c <= 0x2FFFD) ||
         (c >= 0x30000 && c <= 0x3FFFD) || (c >= 0x40000 && c <= 0x4FFFD) ||
         (c >= 0x50000 && c <= 0x5FFFD) || (c >= 0x60000 && c <= 0x6FFFD) ||
         (c >= 0x70000 && c <= 0x7FFFD) || (c >= 0x80000 && c <= 0x8FFFD) ||
         (c >= 0x90000 && c <= 0x9FFFD) || (c >= 0xA0000 && c <= 0xAFFFD) ||
         (c >= 0xB0000 && c <= 0xBFFFD) || (c >= 0xC0000 && c <= 0xCFFFD) ||
         (c >= 0xD0000 && c <= 0xDFFFD) || (c >= 0xE0000 && c <= 0xEFFFD);
}

static auto isValidIdentifierStartCodePoint(uint32_t c) -> bool {
  if (!isValidIdentifierContinuationCodePoint(c)) {
    return false;
  }
  if (c < 0x80 && (clang::isDigit(c) || c == '$')) {
    return false;
  }

  // N1518: Recommendations for extended identifier characters for C and C++
  // Proposed Annex X.2: Ranges of characters disallowed initially
  if ((c >= 0x0300 && c <= 0x036F) || (c >= 0x1DC0 && c <= 0x1DFF) ||
      (c >= 0x20D0 && c <= 0x20FF) || (c >= 0xFE20 && c <= 0xFE2F)) {
    return false;
  }

  return true;
}

static auto advanceIf(char const*& ptr, char const* end,
                      bool (*predicate)(uint32_t)) -> bool {
  char const* next = ptr;
  uint32_t c = validateUTF8CharacterAndAdvance(next, end);
  if (c == ~0U) {
    return false;
  }
  if (predicate(c)) {
    ptr = next;
    return true;
  }
  return false;
}

static auto advanceIfValidStartOfIdentifier(char const*& ptr, char const* end)
    -> bool {
  return advanceIf(ptr, end, isValidIdentifierStartCodePoint);
}

static auto advanceIfValidContinuationOfIdentifier(char const*& ptr,
                                                   char const* end) -> bool {
  return advanceIf(ptr, end, isValidIdentifierContinuationCodePoint);
}

static auto advanceIfValidStartOfOperator(char const*& ptr, char const* end)
    -> bool {
  return advanceIf(ptr, end, Identifier::isOperatorStartCodePoint);
}

static auto advanceIfValidContinuationOfOperator(char const*& ptr,
                                                 char const* end) -> bool {
  return advanceIf(ptr, end, Identifier::isOperatorContinuationCodePoint);
}

static auto diagnoseZeroWidthMatchAndAdvance(char target, const char*& cur_ptr)
    -> bool {
  return *cur_ptr == target && cur_ptr++;
}

static auto advanceIfCustomDelimiter(const char*& cur_ptr) -> unsigned {
  assert(cur_ptr[-1] == '#');

  const char* TmpPtr = cur_ptr;
  unsigned custom_delimiter_len = 1;
  while (diagnoseZeroWidthMatchAndAdvance('#', TmpPtr)) {
    custom_delimiter_len++;
  }
  if (diagnoseZeroWidthMatchAndAdvance('"', TmpPtr)) {
    cur_ptr = TmpPtr;
    return custom_delimiter_len;
  }
  return 0;
}

//===----------------------------------------------------------------------===//
// Operator helper
//===----------------------------------------------------------------------===//

/// 用于确定操作符或token是否在其左侧有边界。
static auto isLeftBound(const char* tok_begin, const char* buffer_begin)
    -> bool {
  if (tok_begin == buffer_begin) {
    return false;
  }

  switch (tok_begin[-1]) {
      // clang-format off
    case ' ': case '\r': case '\n': case '\t': // whitespace
    case '(': case '[': case '{':              // opening delimiters
    case ',': case ';': case ':':              // expression separators
    case '\0':                                 // whitespace / last char in file
      return false;

    case '/':
      if (tok_begin - 1 != buffer_begin &&tok_begin[-2] == '*') {
        return false; // End of a slash-star comment, so whitespace.
      } else {
        return true;
      }
    // 用于检查一个 Unicode 字符 \xA0 是否为左边界的一部分。
    case '\xA0': // \xA0 表示一个特殊的空白字符，即"Non-breaking whitespace"。
      if (tok_begin - 1 != buffer_begin && tok_begin[-2] == '\xC2') {
        return false; // Non-breaking whitespace (U+00A0)
      } else {
        return true;
      }

    default:
      return true;
      // clang-format on
  }
}

/// 确定操作符或token是否在其右侧有边界。
static auto isRightBound(const char* tok_end, bool is_left_bound,
                         const char* code_completion_ptr) -> bool {
  switch (*tok_end) {
      // clang-format off
  case ' ': case '\r': case '\n': case '\t': // whitespace
  case ')': case ']': case '}':              // closing delimiters
  case ',': case ';': case ':':              // expression separators
    return false;

  case '\0':
    if (tok_end == code_completion_ptr)         // code-completion
      return true;
    return false;                            // whitespace / last char in file

  case '.':
    // Prefer the '^' in "x^.y" to be a postfix op, not binary, but the '^' in
    // "^.y" to be a prefix op, not binary.
    return !is_left_bound;

  case '/':
    // A following comment counts as whitespace, so this token is not right bound.
    return tok_end[1] != '/' && tok_end[1] != '*';

  case '\xC2':
    if (tok_end[1] == '\xA0')
      return false; // Non-breaking whitespace (U+00A0)
    else
      return true;

  default:
    return true;
      // clang-format on
  }
}

/// 确定一段文本中是否包含占位符结束标志 #>。
static auto rangeContainsPlaceholderEnd(const char* cur_ptr, const char* end)
    -> bool {
  for (auto sub_str = cur_ptr; sub_str != end - 1; ++sub_str) {
    if (sub_str[0] == '\n') {
      return false;
    }
    if (sub_str[0] == '#' && sub_str[1] == '>') {
      return true;
    }
  }
  return false;
}

//===----------------------------------------------------------------------===//
// Lexer impl
//===----------------------------------------------------------------------===//

Lexer::Lexer(const PrincipalTag&, const LangOptions& lang_opts,
             const SourceManager& src_mgr, unsigned buffer_id,
             LexerMode lex_mode, HashbangMode hashbang_allowed,
             CommentRetentionMode retain_comments)
    : lang_opts_(lang_opts),
      src_mgr_(src_mgr),
      buffer_id_(buffer_id),
      lex_mode_(lex_mode),
      is_hashbang_allowed_(hashbang_allowed == HashbangMode::Allowed),
      retain_comments_(retain_comments) {}

Lexer::Lexer(const LangOptions& options, const SourceManager& src_mgr,
             unsigned buffer_id, LexerMode lex_mode,
             HashbangMode hashbang_allowed,
             CommentRetentionMode retain_comments)
    : Lexer(PrincipalTag(), options, src_mgr, buffer_id, lex_mode,
            hashbang_allowed, retain_comments) {
  unsigned end_offset = src_mgr.getRangeForBuffer(buffer_id).getByteLength();
  initialize(0, end_offset);
}

Lexer::Lexer(const LangOptions& options, const SourceManager& src_mgr,
             unsigned buffer_id, LexerMode lex_mode,
             HashbangMode hashbang_allowed,
             CommentRetentionMode retain_comments, unsigned offset,
             unsigned end_offset)
    : Lexer(PrincipalTag(), options, src_mgr, buffer_id, lex_mode,
            hashbang_allowed, retain_comments) {
  initialize(offset, end_offset);
}

Lexer::Lexer(const Lexer& parent, State begin_state, State end_state)
    : Lexer(PrincipalTag(), parent.lang_opts_, parent.src_mgr_,
            parent.buffer_id_, parent.lex_mode_,
            parent.is_hashbang_allowed_ ? HashbangMode::Allowed
                                        : HashbangMode::Disallowed,
            parent.retain_comments_) {
  assert(buffer_id_ == src_mgr_.findBufferContainingLoc(begin_state.loc_) &&
         "state for the wrong buffer");
  assert(buffer_id_ == src_mgr_.findBufferContainingLoc(end_state.loc_) &&
         "state for the wrong buffer");

  unsigned offset = src_mgr_.getLocOffsetInBuffer(begin_state.loc_, buffer_id_);
  unsigned end_offset =
      src_mgr_.getLocOffsetInBuffer(end_state.loc_, buffer_id_);
  initialize(offset, end_offset);
}

void Lexer::initialize(unsigned offset, unsigned end_offset) {
  assert(offset <= end_offset && "Invalid offset range");

  llvm::StringRef contents =
      src_mgr_.extractText(src_mgr_.getRangeForBuffer(buffer_id_));
  buffer_start_ = contents.data();
  buffer_end_ = buffer_start_ + contents.size();
  assert(*buffer_end_ == 0);
  assert(buffer_start_ + offset <= buffer_end_);
  assert(buffer_start_ + end_offset <= buffer_end_);

  // Check for Unicode BOM at start of file (Only UTF-8 BOM supported now).
  size_t bom_len = contents.startswith("\xEF\xBB\xBF") ? 3 : 0;

  content_start_ = buffer_start_ + bom_len;

  // 初始化代码补全。
  // TODO: code_completion_ptr_ not use.

  cur_ptr_ = buffer_start_ + offset;
  artificial_eof_ = buffer_start_ + end_offset;

  assert(next_token_.is(TokenKind::num_tokens));
  lexImpl();
  assert((next_token_.isAtStartOfLine() || cur_ptr_ != buffer_start_) &&
         "The token should be at the beginning of the line, "
         "or we should be lexing from the middle of the buffer");
}

auto Lexer::getTokenAt(SourceLoc loc) -> Token {
  assert(buffer_id_ ==
             static_cast<unsigned>(src_mgr_.findBufferContainingLoc(loc)) &&
         "location from the wrong buffer");
  Lexer lexer(lang_opts_, src_mgr_, buffer_id_, lex_mode_,
              HashbangMode::Allowed, CommentRetentionMode::None);
  lexer.restoreState(State(loc));
  return lexer.peekNextToken();
}

void Lexer::formToken(TokenKind kind, const char* tok_start) {
  // 当从文件缓冲区的中间对子范围进行lexing时，将会可能超过该范围的末尾，
  // 但仍会处于文件内。
  // 这里检查是否超过了人工EOF，超过则合成一个TokenKind::eof。
  if (kind != TokenKind::eof && artificial_eof_ &&
      tok_start >= artificial_eof_) {
    kind = TokenKind::eof;
  }
  unsigned comment_length = 0;
  if (retain_comments_ == CommentRetentionMode::AttachToNextToken) {
    if (comment_start_) {
      comment_length = cur_ptr_ - comment_start_;
    }
  }

  llvm::StringRef token_text{tok_start,
                             static_cast<size_t>(cur_ptr_ - tok_start)};
  next_token_.setToken(kind, token_text, comment_length);
}

auto Lexer::getStateForBeginningOfTokenLoc(SourceLoc loc) const
    -> Lexer::State {
  const char* ptr = getBufferPtrForSourceLoc(loc);
  // 向后跳过空白，直到遇到换行符。
  // 如果token位于行首，则需要正确地对其进行lex。
  while (ptr >= buffer_start_ + 1) {
    char c = ptr[-1];
    if (c == ' ' || c == '\t') {
      ptr--;
      continue;
    }
    if (c == 0) {
      // NUL字符既可以是whitespace，也可以是code completion token。
      if (ptr - 1 == code_completion_ptr_) {
        break;
      }
      ptr--;
      continue;
    }
    if (c == '\n' || c == '\r') {
      ptr--;
      break;
    }
    break;
  }
  return State(SourceLoc(llvm::SMLoc::getFromPointer(ptr)));
}

void Lexer::skipToEndOfLine(bool eat_newline) {
  bool is_eol = advanceToEndOfLine(cur_ptr_, buffer_end_, code_completion_ptr_);
  if (eat_newline && is_eol) {
    ++cur_ptr_;
    next_token_.setAtStartOfLine(true);
  }
}

void Lexer::skipSlashSlashComment(bool eat_newline) {
  assert(cur_ptr_[-1] == '/' && cur_ptr_[0] == '/' && "Not a // comment");
  skipToEndOfLine(eat_newline);
}

void Lexer::skipHashbang(bool eat_newline) {
  assert(cur_ptr_ == content_start_ && cur_ptr_[0] == '#' &&
         cur_ptr_[1] == '!' && "Not a hashbang");
  skipToEndOfLine(eat_newline);
}

void Lexer::skipSlashStarComment() {
  const char* start_ptr = cur_ptr_ - 1;
  assert(cur_ptr_[-1] == '/' && cur_ptr_[0] == '*' && "Not a /* comment");
  ++cur_ptr_;

  // /* 注释可以被嵌套。
  unsigned depth = 1;

  while (true) {
    switch (*cur_ptr_++) {
      case '*':
        // 如果是'*/'，则减少深度。
        if (cur_ptr_[0] == '/') {
          ++cur_ptr_;
          if (--depth == 0) {
            return;
          }
        }
        break;
      case '/':
        // 如果是'/*'，则增加深度。
        if (cur_ptr_[0] == '*') {
          ++cur_ptr_;
          ++depth;
        }
        break;
      case '\n':
      case '\r':
        next_token_.setAtStartOfLine(true);
        break;
      default:
        break;
    }
  }
}

auto Lexer::getNulCharacterKind(const char* ptr) const
    -> Lexer::NulCharacterKind {
  assert(ptr != nullptr && *ptr == 0);
  if (ptr == code_completion_ptr_) {
    return NulCharacterKind::CodeCompletion;
  }
  if (ptr == buffer_end_) {
    return NulCharacterKind::BufferEnd;
  }
  return NulCharacterKind::Embedded;
}

void Lexer::tryLexEditorPlaceholder() {
  assert(cur_ptr_[-1] == '<' && cur_ptr_[0] == '#');
  const char* tok_start = cur_ptr_ - 1;
  for (const char* ptr = cur_ptr_ + 1; ptr < buffer_end_ - 1; ++ptr) {
    if (*ptr == '\n') {
      break;
    }
    if (ptr[0] == '<' && ptr[1] == '#') {
      // 禁止嵌套。
      break;
    }
    if (*ptr == '#' && ptr[1] == '>') {
      // 找到了placeholder的结束。
      cur_ptr_ = ptr + 2;
      formToken(TokenKind::identifier, tok_start);
      return;
    }
  }
  lexOperatorIdentifier();
}

/// 识别和处理由标点符号构成的操作符标识符。
void Lexer::lexOperatorIdentifier() {
  const char* tok_start = cur_ptr_ - 1;
  cur_ptr_ = tok_start;
  bool did_start = advanceIfValidStartOfOperator(cur_ptr_, buffer_end_);
  assert(did_start && "unexpected operator start");
  (void)did_start;

  do {
    if (cur_ptr_ != buffer_end_ && in_ril_body_ &&
        (*cur_ptr_ == '!' || *cur_ptr_ == '?')) {
      break;
    }

    if (*cur_ptr_ == '.' && *tok_start != '.') {
      break;
    }

    // 如果此时正在lex一个`/.../`正则字面量，则此时不需要考虑`/`字符。
    if (forward_slash_regex_mode_ != LexerForwardSlashRegexMode::None &&
        *cur_ptr_ == '/') {
      break;
    }
  } while (advanceIfValidContinuationOfOperator(cur_ptr_, buffer_end_));

  if (cur_ptr_ - tok_start > 2) {
    // 如果为注释。
    for (const auto *ptr = tok_start + 1; ptr != cur_ptr_ - 1; ++ptr) {
      if (ptr[0] == '/' && (ptr[1] == '/' || ptr[1] == '*')) {
        cur_ptr_ = ptr;
        break;
      }
    }
  }

  bool left_bound = isLeftBound(tok_start, content_start_);
  bool right_bound = isRightBound(cur_ptr_, left_bound, code_completion_ptr_);


}

auto Lexer::lexTrivia(bool is_for_trailing_trivia, const char* all_trivia_start)
    -> llvm::StringRef {
  comment_start_ = nullptr;

restart:
  const char* trivia_start = cur_ptr_;

  switch (*cur_ptr_++) {
    case '\n':  // 换行字符
      if (is_for_trailing_trivia) {
        break;
      }
      next_token_.setAtStartOfLine(true);
      goto restart;
    case '\r':  // 回车字符
      if (is_for_trailing_trivia) {
        break;
      }
      next_token_.setAtStartOfLine(true);
      if (cur_ptr_[0] == '\n') {
        ++cur_ptr_;
      }
      goto restart;
    case ' ':   // 空格符
    case '\t':  // 制表符
    case '\v':  // 垂直制表符
    case '\f':  // 换页符
      goto restart;
    case '/':  // 注释的开始
      if (is_for_trailing_trivia || isKeepingComments()) {
        break;
      } else if (*cur_ptr_ == '/') {
        // 跳过'//'注释。
        if (comment_start_ == nullptr) {
          comment_start_ = cur_ptr_ - 1;
        }
        skipSlashSlashComment(true);
        goto restart;
      } else if (*cur_ptr_ == '*') {
        // 跳过'/*'注释。
        if (comment_start_ == nullptr) {
          comment_start_ = cur_ptr_ - 1;
        }
        skipSlashStarComment();
        goto restart;
      }
      break;
    case '#':  // hashbang
      if (trivia_start == comment_start_ && *cur_ptr_ == '!') {
        --cur_ptr_;
        if (!is_hashbang_allowed_) {
          // TODO: diag
        }
        skipHashbang(false);
        goto restart;
      }
    case '<':  // '<'和'>'是在冲突标记下的特殊字符。
    case '>':
      if (tryLexConflictMarker(false)) {
        goto restart;
      }
      break;
    case 0:  // 用于处理空字符（null）情况。
      switch (getNulCharacterKind(cur_ptr_ - 1)) {
        case NulCharacterKind::Embedded:
          // Embedded NUL characters are lexed as a single space.
          goto restart;
        case NulCharacterKind::CodeCompletion:
        case NulCharacterKind::BufferEnd:
          break;
      }
      break;
      // clang-format off
    case (char)-1: case (char)-2:
    case '@': case '{': case '[': case '(': case '}': case ']': case ')':
    case ',': case ';': case ':': case '\\': case '$':
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '"': case '\'': case '`':
    // Start of identifiers.
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
    case '_':
    // Start of operators.
    case '%': case '!': case '?': case '=':
    case '-': case '+': case '*':
    case '&': case '|': case '^': case '~': case '.':
      break;
      // clang-format on
    default:
      const char* tmp = cur_ptr_ - 1;
      // 确定前一个字符是否是标识符的有效起始字符。
      // 如果是有效的标识符起始字符，那么这个分支会终止并继续词法分析。
      if (advanceIfValidStartOfIdentifier(tmp, buffer_end_)) {
        break;
      }
      // 是否是操作符的有效起始字符。
      if (advanceIfValidStartOfOperator(tmp, buffer_end_)) {
        break;
      }

      bool should_tokenize = lexUnknown();
      if (should_tokenize) {
        cur_ptr_ = tmp;
        size_t length = cur_ptr_ - all_trivia_start;
        return llvm::StringRef(all_trivia_start, length);
      }
      goto restart;
  }
  --cur_ptr_;
  size_t length = cur_ptr_ - all_trivia_start;
  return llvm::StringRef(all_trivia_start, length);
}

void Lexer::lexImpl() {
  // 当前的指针cur_ptr_（可能表示当前正在解析的位置）在有效的范围内，
  // 即它必须在整个代码缓冲区的起始和结束之间。
  assert(cur_ptr_ >= buffer_start_ && cur_ptr_ <= buffer_end_ &&
         "Current pointer out of range!");

  const char* leading_trivial_start = cur_ptr_;
  if (cur_ptr_ == buffer_start_) {
    if (buffer_start_ < content_start_) {
      size_t bom_len = content_start_ - buffer_start_;
      assert(bom_len == 3 && "UTF-8 BOM is 3 bytes");
      cur_ptr_ += bom_len;
    }
    next_token_.setAtStartOfLine(true);
  } else {
    next_token_.setAtStartOfLine(false);
  }

  // 跳过不影响语法的元素。
  lexTrivia(false, leading_trivial_start);

  const char* tok_start = cur_ptr_;

  if (lexer_cut_off_point_ && cur_ptr_ >= lexer_cut_off_point_) {
    return formToken(TokenKind::eof, tok_start);
  }

  switch (*cur_ptr_++) {
    default: {
      const char* tmp = cur_ptr_ - 1;
      if (advanceIfValidStartOfIdentifier(tmp, buffer_end_)) {
        return lexIdentifier();
      }
      if (advanceIfValidStartOfOperator(tmp, buffer_end_)) {
        return lexOperatorIdentifier();
      }
      bool should_tokenize = lexUnknown();
      assert(should_tokenize &&
             "Invalid UTF-8 sequence should be eaten by lexTrivia as "
             "LeadingTrivia");
      (void)should_tokenize;
      return formToken(TokenKind::unknown, tok_start);
    }

    case '\n':
    case '\r':
      llvm_unreachable(
          "Newlines should be eaten by lexTrivia as LeadingTrivia");

    case ' ':
    case '\t':
    case '\f':
    case '\v':
      llvm_unreachable(
          "Whitespaces should be eaten by lexTrivia as LeadingTrivia");

    case (char)-1:  // utf16_bom_marker
    case (char)-2:
      cur_ptr_ = buffer_end_;
      return formToken(TokenKind::unknown, tok_start);

    case 0:
      switch (getNulCharacterKind(cur_ptr_ - 1)) {
        case NulCharacterKind::CodeCompletion:
          while (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_))
            ;
          return formToken(TokenKind::code_complete, tok_start);

        case NulCharacterKind::BufferEnd:
          --cur_ptr_;
          // Return EOF.
          return formToken(TokenKind::eof, tok_start);

        case NulCharacterKind::Embedded:
          llvm_unreachable(
              "Embedded nul should be eaten by lexTrivia as LeadingTrivia");
      }

    case '@':
      return formToken(TokenKind::at_sign, tok_start);
    case '{':
      return formToken(TokenKind::l_brace, tok_start);
    case '[':
      return formToken(TokenKind::l_square, tok_start);
    case '(':
      return formToken(TokenKind::l_paren, tok_start);
    case '}':
      return formToken(TokenKind::r_brace, tok_start);
    case ']':
      return formToken(TokenKind::r_square, tok_start);
    case ')':
      return formToken(TokenKind::r_paren, tok_start);

    case ',':
      return formToken(TokenKind::comma, tok_start);
    case ';':
      return formToken(TokenKind::semi, tok_start);
    case ':':
      return formToken(TokenKind::colon, tok_start);
    case '\\':
      return formToken(TokenKind::backslash, tok_start);

    case '#': {
      // 处理原始字符串字面量。
      if (unsigned custom_delimiter_len = advanceIfCustomDelimiter(cur_ptr_)) {
        return lexStringLiteral(custom_delimiter_len);
      }

      // 可能是正则表达式。
      if (tryLexRegexLiteral(tok_start)) {
        return;
      }

      // magic pound literal：通常指的是以井号 # 开头的特殊字面量
      return lexHash();
    }
    case '/':
      if (cur_ptr_[0] == '/') {  // "//"
        skipSlashSlashComment(true);
        assert(
            isKeepingComments() &&
            "Non token comment should be eaten by lexTrivia as LeadingTrivia");
        return formToken(TokenKind::comment, tok_start);
      }
      if (cur_ptr_[0] == '*') {  // "/*"
        skipSlashStarComment();
        assert(
            isKeepingComments() &&
            "Non token comment should be eaten by lexTrivia as LeadingTrivia");
        return formToken(TokenKind::comment, tok_start);
      }
      if (tryLexRegexLiteral(tok_start)) {
        return;
      }
      // 最后才判断是否是操作符。
      return lexOperatorIdentifier();
    case '%':  // local RIL value
      // lex %[0-9a-zA-Z_]+
      if (in_ril_body_ && clang::isAsciiIdentifierContinue(cur_ptr_[0])) {
        do {
          ++cur_ptr_;
        } while (clang::isAsciiIdentifierContinue(cur_ptr_[0]));

        return formToken(TokenKind::ril_local_name, tok_start);
      }
      return lexOperatorIdentifier();

    case '!':
      if (in_ril_body_) {
        return formToken(TokenKind::ril_exclamation, tok_start);
      }
      if (isLeftBound(tok_start, content_start_)) {
        return formToken(TokenKind::exclaim_postfix, tok_start);
      }
      return lexOperatorIdentifier();

    case '?':
      if (isLeftBound(tok_start, content_start_)) {
        return formToken(TokenKind::question_postfix, tok_start);
      }
      return lexOperatorIdentifier();

    case '<':
      if (cur_ptr_[0] == '#') {
        return tryLexEditorPlaceholder();
      }
      return lexOperatorIdentifier();
    case '>':
      return lexOperatorIdentifier();

      // clang-format off
    case '=': case '-': case '+': case '*':
    case '&': case '|':  case '^': case '~': case '.':
      return lexOperatorIdentifier();

    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
    case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
    case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U':
    case 'V': case 'W': case 'X': case 'Y': case 'Z':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u':
    case 'v': case 'w': case 'x': case 'y': case 'z':
    case '_':
      return lexIdentifier();

    case '$':
      return lexDollarIdentifier();

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return lexNumber();

    case '\'':
    case '"':
      return lexStringLiteral();

    case '`':
      return lexEscapedIdentifier();
      // clang-format on
  }
}

}  // namespace Radium