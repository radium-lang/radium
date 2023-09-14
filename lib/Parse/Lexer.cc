#include "Radium/Parse/Lexer.h"

#include <llvm-15/llvm/ADT/StringRef.h>
#include <llvm-15/llvm/ADT/StringSwitch.h>
#include <llvm-15/llvm/Support/Compiler.h>

#include "Radium/AST/Identifier.h"
#include "Radium/Basic/Fallthrough.h"
#include "Radium/Basic/LangOptions.h"
#include "Radium/Basic/SourceManager.h"
#include "Radium/Parse/Token.h"
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
// advance and character helper
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

/// 判断一个给定的Unicode码点是否是一个有效的标识符继续字符。
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
  // TODO: diag
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

/// 检查是否存在有效的省略换行符转义。
static auto maybeConsumeNewlineEscape(const char*& cur_ptr, ssize_t offset)
    -> bool {
  const char* tmp_ptr = cur_ptr + offset;
  while (true) {
    switch (*tmp_ptr++) {
      case ' ':
      case '\t':
        continue;
      case '\r':
        if (*tmp_ptr == '\n') {
          ++tmp_ptr;
        }
        LLVM_FALLTHROUGH;
      case '\n':
        cur_ptr = tmp_ptr;
        return true;
      case 0:
      default:
        return false;
    }
  }
}

/// 检查一个字符串是否以给定数量的 '#' 字符开始，
/// 并检查''后面的 '#' 字符数量是否与之匹配。
/// custom_delimiter_len代表期望的`#`字符数。
/// byte_ptr代表要检查的字符串的指针。
/// is_closing表示是否正在查找字符串的结束分隔符。
static auto delimiterMatches(unsigned custom_delimiter_len,
                             const char*& bytes_ptr, bool is_closing = false)
    -> bool {
  if (!custom_delimiter_len) {
    return true;
  }
  const char* tmp_ptr = bytes_ptr;
  while (diagnoseZeroWidthMatchAndAdvance('#', tmp_ptr)) {
  }

  if (tmp_ptr - bytes_ptr < custom_delimiter_len) {
    return false;
  }

  // TODO: diag
  return true;
}

/// 检查一个字符串是否包含多行字符串分隔符。
static auto advanceIfMultilineDelimiter(unsigned custom_delimiter_len,
                                        const char*& cur_ptr,
                                        bool is_opening = false) -> bool {
  const char* tmp_ptr = cur_ptr + 1;
  if (is_opening && custom_delimiter_len) {
    while (*tmp_ptr != '\r' && *tmp_ptr != '\n') {
      if (*tmp_ptr == '"') {
        if (delimiterMatches(custom_delimiter_len, ++tmp_ptr)) {
          cur_ptr = tmp_ptr + custom_delimiter_len + 1;
          return true;
        }
        continue;
      }
      ++tmp_ptr;
    }
  }

  tmp_ptr = cur_ptr;
  if (*(tmp_ptr - 1) == '"' && diagnoseZeroWidthMatchAndAdvance('"', tmp_ptr) &&
      diagnoseZeroWidthMatchAndAdvance('"', tmp_ptr)) {
    cur_ptr = tmp_ptr;
    return true;
  }

  return false;
}

//===----------------------------------------------------------------------===//
// Operator helper
//===----------------------------------------------------------------------===//

/// 用于确定操作符或token是否在其左侧有边界。
static auto isLeftBound(const char* tok_begin, const char* buffer_begin)
    -> bool {
  // 如果token位于缓存区的开始，则没有左边界。
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

/// 跳过并识别/* ... */风格的块注释。这种注释可以跨越多行，并且可以嵌套。
static auto skipToEndOfSlashStarComment(
    const char*& cur_ptr, const char* buffer_end,
    const char* code_completion_ptr = nullptr) -> bool {
  const char* start_ptr = cur_ptr - 1;
  assert(cur_ptr[-1] == '/' && cur_ptr[0] == '*' && "Not a /* comment");

  ++cur_ptr;

  // /**/ comments can be nested, keep track of how deep we've gone.
  unsigned depth = 1;
  bool is_multiline = false;

  while (true) {
    switch (*cur_ptr++) {
      case '*':
        // Check for a '*/'
        if (*cur_ptr == '/') {
          ++cur_ptr;
          if (--depth == 0) {
            return is_multiline;
          }
        }
        break;
      case '/':
        // Check for a '/*'
        if (*cur_ptr == '*') {
          ++cur_ptr;
          ++depth;
        }
        break;

      case '\n':
      case '\r':
        is_multiline = true;
        break;

      default:
        // If this is a "high" UTF-8 character, validate it.
        if ((signed char)(cur_ptr[-1]) < 0) {
          --cur_ptr;
          const char* char_start = cur_ptr;
        }

        break;  // Otherwise, eat other characters.
      case 0:
        if (cur_ptr - 1 != buffer_end) {
          continue;
        }
        // Otherwise, we have an unterminated /* comment.
        --cur_ptr;

        return is_multiline;
    }
  }
}

/// 在字符串字面量中找到插值表达式的结束位置。
/// 插值表达式是像这样的东西：\(...)，其中...是要插入字符串的任何表达式。
static auto skipToEndOfInterpolatedExpression(const char* cur_ptr,
                                              const char* end_ptr,
                                              bool is_multiline_string) -> const
    char* {
  SmallVector<char, 4> open_delimiters;
  SmallVector<bool, 4> allow_newline;
  SmallVector<unsigned, 4> custom_delimiter;
  allow_newline.push_back(is_multiline_string);

  auto in_string_literal = [open_delimiters]() {
    return !open_delimiters.empty() &&
           (open_delimiters.back() == '"' || open_delimiters.back() == '\'');
  };
  while (true) {
    // 在插值字符串中，表达式不能跨越多行。
    unsigned custom_delimiter_len = 0;
    switch (*cur_ptr++) {
      case '\n':
      case '\r':
        if (allow_newline.back()) {
          continue;
        }
        return cur_ptr - 1;
      case 0:
        if (cur_ptr - 1 != end_ptr)
          continue;
        return cur_ptr - 1;

      case '#':
        if (in_string_literal() ||
            !(custom_delimiter_len = advanceIfCustomDelimiter(cur_ptr)))
          continue;
        assert(cur_ptr[-1] == '"' &&
               "advanceIfCustomDelimiter() must stop at after the quote");
        LLVM_FALLTHROUGH;

      case '"':
      case '\'': {
        if (!in_string_literal()) {
          // Open string literal.
          open_delimiters.push_back(cur_ptr[-1]);
          allow_newline.push_back(
              advanceIfMultilineDelimiter(custom_delimiter_len, cur_ptr, true));
          custom_delimiter.push_back(custom_delimiter_len);
          continue;
        }

        // In string literal.

        // Skip if it's an another kind of quote in string literal. e.g.
        // "foo's".
        if (open_delimiters.back() != cur_ptr[-1])
          continue;

        // Multi-line string can only be closed by '"""'.
        if (allow_newline.back() &&
            !advanceIfMultilineDelimiter(custom_delimiter_len, cur_ptr))
          continue;

        // Check whether we have equivalent number of '#'s.
        if (!delimiterMatches(custom_delimiter.back(), cur_ptr, true))
          continue;

        // Close string literal.
        open_delimiters.pop_back();
        allow_newline.pop_back();
        custom_delimiter.pop_back();
        continue;
      }
      case '\\':
        // We ignore invalid escape sequence here. They should be diagnosed in
        // the real lexer functions.
        if (in_string_literal() &&
            delimiterMatches(custom_delimiter.back(), cur_ptr)) {
          switch (*cur_ptr++) {
            case '(':
              // Entering a recursive interpolated expression
              open_delimiters.push_back('(');
              continue;
            case '\n':
            case '\r':
            case 0:
              // Don't jump over newline/EOF due to preceding backslash.
              // Let the outer switch to handle it.
              --cur_ptr;
              continue;
            default:
              continue;
          }
        }
        continue;

      // Paren nesting deeper to support "foo = \((a+b)-(c*d)) bar".
      case '(':
        if (!in_string_literal()) {
          open_delimiters.push_back('(');
        }
        continue;
      case ')':
        if (open_delimiters.empty()) {
          // No outstanding open delimiters; we're done.
          return cur_ptr - 1;
        } else if (open_delimiters.back() == '(') {
          // Pop the matching bracket and keep going.
          open_delimiters.pop_back();
          continue;
        } else {
          // It's a right parenthesis in a string literal.
          assert(in_string_literal());
          continue;
        }
      case '/':
        if (in_string_literal())
          continue;

        if (*cur_ptr == '*') {
          auto CommentStart = cur_ptr - 1;
          bool isMultilineComment =
              skipToEndOfSlashStarComment(cur_ptr, end_ptr);
          if (isMultilineComment && !allow_newline.back()) {
            // Multiline comment is prohibited in string literal.
            // Return the start of the comment.
            return CommentStart;
          }
        } else if (*cur_ptr == '/') {
          if (!allow_newline.back()) {
            // '//' comment is impossible in single line string literal.
            // Return the start of the comment.
            return cur_ptr - 1;
          }
          // Advance to the end of the comment.
          if (/*isEOL=*/advanceToEndOfLine(cur_ptr, end_ptr))
            ++cur_ptr;
        }
        continue;
      default:
        // Normal token character.
        continue;
    }
  }
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

void Lexer::formEscapedIdentifierToken(const char* tok_start) {
  assert(cur_ptr_ - tok_start >= 3 &&
         "escaped identifier must be longer than or equal 3 bytes");
  assert(tok_start[0] == '`' && "escaped identifier starts with backtick");
  assert(cur_ptr_[-1] == '`' && "escaped identifier ends with backtick");

  formToken(TokenKind::identifier, tok_start);
  if (next_token_.is(TokenKind::eof)) {
    return;
  }
  next_token_.setEscapedIdentifier(true);
}

void Lexer::formStringLiteralToken(const char* tok_start,
                                   bool is_multiline_string,
                                   unsigned custom_delimiter_len) {
  formToken(TokenKind::string_literal, tok_start);
  if (next_token_.is(TokenKind::eof)) {
    return;
  }
  next_token_.setStringLiteral(is_multiline_string, custom_delimiter_len);
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
  // const char* start_ptr = cur_ptr_ - 1;
  // assert(cur_ptr_[-1] == '/' && cur_ptr_[0] == '*' && "Not a /* comment");
  // ++cur_ptr_;
  // // /* 注释可以被嵌套。
  // unsigned depth = 1;
  // while (true) {
  //   switch (*cur_ptr_++) {
  //     case '*':
  //       // 如果是'*/'，则减少深度。
  //       if (cur_ptr_[0] == '/') {
  //         ++cur_ptr_;
  //         if (--depth == 0) {
  //           return;
  //         }
  //       }
  //       break;
  //     case '/':
  //       // 如果是'/*'，则增加深度。
  //       if (cur_ptr_[0] == '*') {
  //         ++cur_ptr_;
  //         ++depth;
  //       }
  //       break;
  //     case '\n':
  //     case '\r':
  //       next_token_.setAtStartOfLine(true);
  //       break;
  //     default:
  //       break;
  //   }
  // }

  bool is_multiline =
      skipToEndOfSlashStarComment(cur_ptr_, buffer_end_, code_completion_ptr_);
  if (is_multiline) {
    next_token_.setAtStartOfLine(true);
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

auto Lexer::isOperator(llvm::StringRef string) -> bool {
  if (string.empty()) {
    return false;
  }
  const char* p = string.data();
  const char* end = string.end();
  if (!advanceIfValidStartOfOperator(p, end)) {
    return false;
  }
  while (p < end && advanceIfValidContinuationOfOperator(p, end))
    ;
  return p == end;
}

auto Lexer::kindOfIdentifier(llvm::StringRef identifier, bool in_ril_mode)
    -> TokenKind {
#define RADIUM_RIL_KEYWORD(Name)
#define RADIUM_KEYWORD(Name) \
  if (identifier == #Name)   \
    return TokenKind::kw_##Name;
#include "Radium/Parse/TokenKinds.def"

  if (in_ril_mode) {
#define RADIUM_RIL_KEYWORD(Name) \
  if (identifier == #Name)       \
    return TokenKind::kw_##Name;
#include "Radium/Parse/TokenKinds.def"
  }
  return TokenKind::identifier;
}

/// [a-zA-Z_][a-zA-Z0-9_$]*
void Lexer::lexIdentifier() {
  const char* tok_start = cur_ptr_ - 1;
  cur_ptr_ = tok_start;
  bool did_start = advanceIfValidStartOfIdentifier(cur_ptr_, buffer_end_);
  assert(did_start && "Unexpected identifier start");
  (void)did_start;

  while (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_))
    ;

  TokenKind kind =
      kindOfIdentifier(llvm::StringRef(tok_start, cur_ptr_ - tok_start),
                       lex_mode_ == LexerMode::RIL);
  return formToken(kind, tok_start);
}

/// 用于处理解析以`#`开头的token。
void Lexer::lexHash() {
  const char* tok_start = cur_ptr_ - 1;  // #

  // 扫描#后面的[a-zA-Z]+
  const char* tmp_ptr = cur_ptr_;
  if (clang::isAsciiIdentifierStart(*tmp_ptr)) {
    do {
      ++tmp_ptr;
    } while (clang::isAsciiIdentifierContinue(*tmp_ptr));
  }

  // 映射字符序列到token类型。
  TokenKind kind = llvm::StringSwitch<TokenKind>(
                       llvm::StringRef(cur_ptr_, tmp_ptr - cur_ptr_))
#define RADIUM_POUND_KEYWORD(id) .Case(#id, TokenKind::pound_##id)
#include "Radium/Parse/TokenKinds.def"
                       .Default(TokenKind::pound);

  // 处理特定的类型。
  // TODO: pound_assert

  if (kind == TokenKind::pound) {
    return formToken(TokenKind::pound, tok_start);
  }

  cur_ptr_ = tmp_ptr;
  return formToken(kind, tok_start);
}

/// lexEscapedIdentifier:
///   identifier ::= '`' identifier '`'
void Lexer::lexEscapedIdentifier() {
  assert(cur_ptr_[-1] == '`' && "not an escaped identifier");
  const char* quote = cur_ptr_ - 1;

  const char* identifier_start = cur_ptr_;
  if (advanceIfValidStartOfIdentifier(cur_ptr_, buffer_end_)) {
    while (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_))
      ;

    if (*cur_ptr_ == '`') {
      ++cur_ptr_;
      formEscapedIdentifierToken(quote);
      return;
    }
  }

  if (quote[1] == '$' && quote[2] == '`') {
    cur_ptr_ = quote + 3;
    formEscapedIdentifierToken(quote);
    return;
  }

  cur_ptr_ = identifier_start;
  formToken(TokenKind::backtick, quote);
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
    for (const auto* ptr = tok_start + 1; ptr != cur_ptr_ - 1; ++ptr) {
      if (ptr[0] == '/' && (ptr[1] == '/' || ptr[1] == '*')) {
        cur_ptr_ = ptr;
        break;
      }
    }
  }

  // 用于确定操作符是左边界还是右边界。
  bool left_bound = isLeftBound(tok_start, content_start_);
  bool right_bound = isRightBound(cur_ptr_, left_bound, code_completion_ptr_);

  // 匹配多种保留的操作符，包括单字符操作符和两字符操作符。
  if (cur_ptr_ - tok_start == 1) {
    switch (tok_start[0]) {
      case '=':
        return formToken(TokenKind::equal, tok_start);
      case '&':
        return formToken(TokenKind::amp_prefix, tok_start);
      case '.': {
        // 如果`.`左右两边都有边界（例如，它可能位于两个标识符之间），则将其识别为一个普通的`.`。
        if (left_bound == right_bound) {
          return formToken(TokenKind::period, tok_start);
        }
        if (right_bound) {
          return formToken(TokenKind::period_prefix, tok_start);
        }

        // 检查水平空白。
        const char* after_horz_whitespace = cur_ptr_;
        while (*after_horz_whitespace == ' ' ||
               *after_horz_whitespace == '\t') {
          ++after_horz_whitespace;
        }

        // 处理代码补全情况。
        // 例如输入"x. <ESC>"，这段代码确保返回一个`.`token。
        if (*after_horz_whitespace == '\0' &&
            after_horz_whitespace == code_completion_ptr_) {
          // TODO: diag
          return formToken(TokenKind::period, tok_start);
        }

        // 处理额外空白情况。
        // 如果`.`后面的空白之后是一个右边界，并且不是注释的开始（//或/*），则诊断为额外的空白，并建议删除它。
        if (isRightBound(after_horz_whitespace, left_bound,
                         code_completion_ptr_) &&
            *after_horz_whitespace != '/') {
          // TODO: diag, remove char
          return formToken(TokenKind::period, tok_start);
        }

        return formToken(TokenKind::unknown, tok_start);
      }
      case '?':
        if (left_bound) {
          return formToken(TokenKind::question_postfix, tok_start);
        }
        return formToken(TokenKind::question_infix, tok_start);
    }
  } else if (cur_ptr_ - tok_start == 2) {
    // 将两个字符组合成整数。
    switch ((tok_start[0] << 8) | tok_start[1]) {
      case ('-' << 8) | '>':  // ->
        return formToken(TokenKind::arrow, tok_start);
      case ('*' << 8) | '/':  // */
        // TODO: diag
        return formToken(TokenKind::unknown, tok_start);
    }
  } else {
    // 确保在标识符token中没有"*/"这样的字符组合。
    auto pos = llvm::StringRef(tok_start, cur_ptr_ - tok_start).find("*/");
    if (pos != llvm::StringRef::npos) {
      return formToken(TokenKind::unknown, tok_start);
    }
  }

  if (left_bound == right_bound) {
    return formToken(left_bound ? TokenKind::oper_binary_unspaced
                                : TokenKind::oper_binary_spaced,
                     tok_start);
  }
  return formToken(
      left_bound ? TokenKind::oper_postfix : TokenKind::oper_prefix, tok_start);
}

/// $[0-9a-zA-Z_$]+
void Lexer::lexDollarIdentifier() {
  const char* tok_start = cur_ptr_ - 1;
  assert(tok_start[0] == '$' && "Not a dollar identifier");

  // 处理RIL函数体中的$标识符。
  if (in_ril_body_ && next_token_.getKind() != TokenKind::at_sign) {
    return formToken(TokenKind::ril_dollar, tok_start);
  }

  bool is_all_digits = true;
  while (true) {
    if (clang::isDigit(*cur_ptr_)) {
      ++cur_ptr_;
      continue;
    } else if (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_)) {
      is_all_digits = false;
      continue;
    }
    break;
  }

  if (cur_ptr_ == tok_start + 1) {
    return formToken(TokenKind::identifier, tok_start);
  }

  if (!is_all_digits) {
    return formToken(TokenKind::identifier, tok_start);
  } else {
    return formToken(TokenKind::dollarident, tok_start);
  }
}

enum class ExpectDigitKind : unsigned { Binary, Octal, Decimal, Hex };

/// 解析十六进制数（`0x`引导）。
void Lexer::lexHexNumber() {
  // 根据`0x`引导进行断言。
  assert(*cur_ptr_ == 'x' && "not a hex literal");
  const char* tok_start = cur_ptr_ - 1;
  assert(*tok_start == '0' && "not a hex literal");

  auto expected_digit = [&]() {
    while (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_))
      ;
    return formToken(TokenKind::unknown, tok_start);
  };

  auto expected_hex_digit = [&]() {
    // TODO: diag
    return expected_digit();
  };

  // 0x[0-9a-fA-F][0-9a-fA-F_]*
  ++cur_ptr_;
  if (!clang::isHexDigit(*cur_ptr_)) {
    return expected_hex_digit();
  }
  // 下划线在Radium中用于数字字面量的可读性。
  while (clang::isHexDigit(*cur_ptr_) || *cur_ptr_ == '_') {
    ++cur_ptr_;
  }

  // 检查是否浮点数。
  // Radium里只用p表示指数，和carbon能选择用e或p不一样。
  if (*cur_ptr_ != '.' && *cur_ptr_ != 'p' && *cur_ptr_ != 'P') {
    const auto* tmp = cur_ptr_;
    if (advanceIfValidContinuationOfIdentifier(tmp, buffer_end_)) {
      return expected_hex_digit();
    } else {
      return formToken(TokenKind::integer_literal, tok_start);
    }
  }

  const char* ptr_on_dot = nullptr;

  // 解析小数部分。
  // (\.[0-9A-Fa-f][0-9A-Fa-f_]*)?
  if (*cur_ptr_ == '.') {
    ptr_on_dot = cur_ptr_;
    ++cur_ptr_;
    if (!clang::isHexDigit(*cur_ptr_)) {
      ++cur_ptr_;
      return formToken(TokenKind::integer_literal, tok_start);
    }

    while (clang::isHexDigit(*cur_ptr_) || *cur_ptr_ == '_') {
      ++cur_ptr_;
    }

    if (*cur_ptr_ != 'p' && *cur_ptr_ != 'P') {
      if (!clang::isDigit(ptr_on_dot[1])) {
        cur_ptr_ = ptr_on_dot;
        return formToken(TokenKind::integer_literal, tok_start);
      }
      // TODO: diag
      return formToken(TokenKind::unknown, tok_start);
    }
  }

  // 解析指数部分。
  // [pP][+-]?[0-9][0-9_]*
  assert(*cur_ptr_ == 'p' || *cur_ptr_ == 'P' && "not a hex float exponent");
  ++cur_ptr_;

  bool signed_exponent = false;
  if (*cur_ptr_ == '+' || *cur_ptr_ == '-') {
    signed_exponent = true;
    ++cur_ptr_;
  }

  // 检查指数的下一个字符是否是数字。
  if (!clang::isDigit(*cur_ptr_)) {
    // 检查小数点后的字符是否是数字，并且指数没有符号，例如 0xff.fpValue,
    // 0xff.fp。
    if (ptr_on_dot && clang::isDigit(ptr_on_dot[1]) && !signed_exponent) {
      cur_ptr_ = ptr_on_dot;
      return formToken(TokenKind::integer_literal, tok_start);
    }

    const auto* tmp = cur_ptr_;
    if (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_)) {
      // TODO: diag
    }

    return expected_digit();
  }

  while (clang::isDigit(*cur_ptr_) || *cur_ptr_ == '_') {
    ++cur_ptr_;
  }

  const auto* tmp = cur_ptr_;
  if (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_)) {
    return expected_digit();
  }

  return formToken(TokenKind::floating_literal, tok_start);
}

/// lexNumber:
///   integer_literal  ::= [0-9][0-9_]*
///   integer_literal  ::= 0x[0-9a-fA-F][0-9a-fA-F_]*
///   integer_literal  ::= 0o[0-7][0-7_]*
///   integer_literal  ::= 0b[01][01_]*
///   floating_literal ::= [0-9][0-9]_*\.[0-9][0-9_]*
///   floating_literal ::= [0-9][0-9]*\.[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
///   floating_literal ::= [0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
///   floating_literal ::= 0x[0-9A-Fa-f][0-9A-Fa-f_]*
///                          (\.[0-9A-Fa-f][0-9A-Fa-f_]*)?[pP][+-]?[0-9][0-9_]*
void Lexer::lexNumber() {
  const char* tok_start = cur_ptr_ - 1;
  assert((clang::isDigit(*tok_start) || *tok_start == '.') &&
         "Unexpected start");

  // 用于处理无效整数字符，一直尝试前进直到不符合匹配。
  auto expected_digit = [&]() {
    while (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_))
      ;
    return formToken(TokenKind::unknown, tok_start);
  };

  auto expected_int_digit = [&](const char* loc, ExpectDigitKind kind) {
    // TODO: diag
    return expected_digit();
  };

  // 16进制。
  if (*tok_start == '0' && *cur_ptr_ == 'x') {
    return lexHexNumber();
  }

  // 8进制：0o[0-7][0-7_]*
  if (*tok_start == '0' && *cur_ptr_ == 'o') {
    ++cur_ptr_;
    if (*cur_ptr_ < '0' || *cur_ptr_ > '7') {
      return expected_int_digit(cur_ptr_, ExpectDigitKind::Octal);
    }

    while ((*cur_ptr_ >= '0' && *cur_ptr_ <= '7') || *cur_ptr_ == '_') {
      ++cur_ptr_;
    }

    const auto* tmp = cur_ptr_;
    if (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_)) {
      return expected_int_digit(tmp, ExpectDigitKind::Octal);
    }

    return formToken(TokenKind::integer_literal, tok_start);
  }

  while (clang::isDigit(*cur_ptr_) || *cur_ptr_ == '_') {
    ++cur_ptr_;
  }

  if (*cur_ptr_ == '.') {
    if (!clang::isDigit(cur_ptr_[1]) || next_token_.is(TokenKind::period)) {
      return formToken(TokenKind::integer_literal, tok_start);
    }
  } else {
    if (*cur_ptr_ != 'e' && *cur_ptr_ != 'E') {
      const auto* tmp = cur_ptr_;
      if (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_)) {
        return expected_int_digit(tmp, ExpectDigitKind::Decimal);
      }
      return formToken(TokenKind::integer_literal, tok_start);
    }
  }

  if (*cur_ptr_ == '.') {
    ++cur_ptr_;

    while (clang::isDigit(*cur_ptr_) || *cur_ptr_ == '_') {
      ++cur_ptr_;
    }
  }

  if (*cur_ptr_ == 'e' || *cur_ptr_ == 'E') {
    ++cur_ptr_;
    if (*cur_ptr_ == '+' || *cur_ptr_ == '-') {
      ++cur_ptr_;
    }

    if (!clang::isDigit(*cur_ptr_)) {
      const auto* tmp = cur_ptr_;
      if (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_)) {
        // TODO: diag
      }
      return expected_digit();
    }

    while (clang::isDigit(*cur_ptr_) || *cur_ptr_ == '_') {
      ++cur_ptr_;
    }

    const auto* tmp = cur_ptr_;
    if (advanceIfValidContinuationOfIdentifier(cur_ptr_, buffer_end_)) {
      // TODO:: diag
      return expected_digit();
    }
  }
  return formToken(TokenKind::floating_literal, tok_start);
}

///   unicode_character_escape ::= [\]u{hex+}
///   hex                      ::= [0-9a-fA-F]
/// 解析并验证Unicode字符转义序列。
auto Lexer::lexUnicodeEscape(const char*& cur_ptr) -> unsigned {
  // 确保当前的字符是`{`，这是Unicode转义序列的开始
  assert(cur_ptr[0] == '{' && "Invalid unicode escape");
  ++cur_ptr;

  const char* digit_start = cur_ptr;

  unsigned num_digits = 0;
  for (; clang::isHexDigit(cur_ptr[0]); ++num_digits) {
    ++cur_ptr;
  }

  if (cur_ptr[0] != '}') {
    // TODO: diag
    return ~1U;
  }
  ++cur_ptr;

  if (num_digits < 1 || num_digits > 8) {
    // TODO: diag
    return ~1U;
  }

  unsigned char_value = 0;
  llvm::StringRef(digit_start, num_digits).getAsInteger(16, char_value);
  return char_value;
}

auto Lexer::lexCharacter(const char*& cur_ptr, char stop_quote,
                         bool is_multiline_string,
                         unsigned custom_delimiter_len) -> unsigned {
  const char* char_start = cur_ptr;

  switch (*cur_ptr++) {
    default: {  // Normal characters are part of the string.
      // Normal characters are part of the string.
      // If this is a "high" UTF-8 character, validate it.
      if ((signed char)(cur_ptr[-1]) >= 0) {
        return cur_ptr[-1];
      }
      --cur_ptr;
      unsigned char_value =
          validateUTF8CharacterAndAdvance(cur_ptr, buffer_end_);
      if (char_value != ~0U)
        return char_value;
      return ~1U;
    }
    case '"':
    case '\'':
      if (cur_ptr[-1] == stop_quote) {
        // Multiline and custom escaping are only enabled for " quote.
        if (LLVM_UNLIKELY(stop_quote != '"'))
          return ~0U;
        if (!is_multiline_string && !custom_delimiter_len)
          return ~0U;

        auto tmp_ptr = cur_ptr;
        if (is_multiline_string &&
            !advanceIfMultilineDelimiter(custom_delimiter_len, tmp_ptr))
          return '"';
        if (custom_delimiter_len &&
            !delimiterMatches(custom_delimiter_len, tmp_ptr,
                              /*IsClosing=*/true))
          return '"';
        cur_ptr = tmp_ptr;
        return ~0U;
      }
      // Otherwise, this is just a character.
      return cur_ptr[-1];

    case 0:
      assert(cur_ptr - 1 != buffer_end_ && "Caller must handle EOF");
      return cur_ptr[-1];
    case '\n':  // String literals cannot have \n or \r in them.
    case '\r':
      assert(is_multiline_string &&
             "Caller must handle newlines in non-multiline");
      return cur_ptr[-1];
    case '\\':  // Escapes.
      if (!delimiterMatches(custom_delimiter_len, cur_ptr))
        return '\\';
      break;
  }

  unsigned char_value = 0;
  // Escape processing.  We already ate the "\".
  switch (*cur_ptr) {
    case ' ':
    case '\t':
    case '\n':
    case '\r':
      if (is_multiline_string && maybeConsumeNewlineEscape(cur_ptr, 0))
        return '\n';
      LLVM_FALLTHROUGH;
    default:  // Invalid escape.
      // If this looks like a plausible escape character, recover as though this
      // is an invalid escape.
      if (clang::isAlphanumeric(*cur_ptr))
        ++cur_ptr;
      return ~1U;

    // Simple single-character escapes.
    case '0':
      ++cur_ptr;
      return '\0';
    case 'n':
      ++cur_ptr;
      return '\n';
    case 'r':
      ++cur_ptr;
      return '\r';
    case 't':
      ++cur_ptr;
      return '\t';
    case '"':
      ++cur_ptr;
      return '"';
    case '\'':
      ++cur_ptr;
      return '\'';
    case '\\':
      ++cur_ptr;
      return '\\';

    case 'u': {  //  \u HEX HEX HEX HEX
      ++cur_ptr;
      if (*cur_ptr != '{') {
        return ~1U;
      }

      char_value = lexUnicodeEscape(cur_ptr);
      if (char_value == ~1U)
        return ~1U;
      break;
    }
  }

  // Check to see if the encoding is valid.
  llvm::SmallString<64> temp_string;
  if (char_value >= 0x80 && EncodeToUTF8(char_value, temp_string)) {
    return ~1U;
  }

  return char_value;
}

/// lexStringLiteral:
///   string_literal ::= ["]([^"\\\n\r]|character_escape)*["] // 单行
///   string_literal ::= ["]["]["].*["]["]["] - approximately // 多行
///   string_literal ::= (#+)("")?".*"(\2\1) - "raw" strings  // raw: ##"str"##
void Lexer::lexStringLiteral(unsigned custom_delimiter_len) {
  const char quote_char = cur_ptr_[-1];
  const char* tok_start = cur_ptr_ - 1 - custom_delimiter_len;

  assert((quote_char == '"' || quote_char == '\'') && "Unexpected quote char");
  bool is_multiline_string =
      advanceIfMultilineDelimiter(custom_delimiter_len, cur_ptr_, true);
  // TODO: diag

  bool was_erroneous = false;
  while (true) {
    const char* tmp_ptr = cur_ptr_ + 1;
    if (*cur_ptr_ == '\\' && delimiterMatches(custom_delimiter_len, tmp_ptr) &&
        *tmp_ptr++ == '(') {
      cur_ptr_ = skipToEndOfInterpolatedExpression(tmp_ptr, buffer_end_,
                                                   is_multiline_string);

      if (*cur_ptr_ == ')') {
        ++cur_ptr_;
        continue;
      } else {
        if ((*cur_ptr_ == '\r' || *cur_ptr_ == '\n') && is_multiline_string) {
          was_erroneous = true;
          continue;
        } else if (!is_multiline_string || cur_ptr_ == buffer_end_) {
          // TODO: diag
        }
        return formToken(TokenKind::unknown, tok_start);
      }
    }

    if (((*cur_ptr_ == '\r' || *cur_ptr_ == '\n') && is_multiline_string) ||
        cur_ptr_ == buffer_end_) {
      return formToken(TokenKind::unknown, tok_start);
    }

    unsigned char_value = lexCharacter(cur_ptr_, quote_char);
    if (char_value == ~0U) {
      break;
    }
    was_erroneous |= char_value == ~1U;
  }

  if (was_erroneous) {
    return formToken(TokenKind::unknown, tok_start);
  }

  return formStringLiteralToken(tok_start, is_multiline_string,
                                custom_delimiter_len);
}

/// 处理未知或者非法字符。
/// 如无效的UTF-8字符、非换行空格、花括号引号和混淆的字符。
auto Lexer::lexUnknown() -> bool {
  const char* tmp = cur_ptr_ - 1;

  if (advanceIfValidContinuationOfIdentifier(tmp, buffer_end_)) {
    while (advanceIfValidContinuationOfOperator(tmp, buffer_end_))
      ;
    cur_ptr_ = tmp;
    return true;
  }

  uint32_t code_point = validateUTF8CharacterAndAdvance(tmp, buffer_end_);
  if (code_point == ~0U) {
    cur_ptr_ = tmp;
    return false;
  } else if (code_point == 0x000000A0) {  // 检查码点是否为非换行空格(U+00A0)。
    while (tmp[0] == '\xC2' && tmp[1] == '\xA0') {
      tmp += 2;
    }
    llvm::SmallString<8> spaces;
    spaces.assign((tmp - cur_ptr_ + 1) / 2, ' ');
    cur_ptr_ = tmp;
    return false;
  } else if (code_point ==
             0x0000201D) {  // 检查码点是否为结束的花括号引号(U+201D)。
    cur_ptr_ = tmp;
    return true;
  } else if (code_point ==
             0x0000201C) {  // 检查码点是否为开始的花括号引号(U+201C)。
    // TODO: do fuzzy match.
    cur_ptr_ = tmp;
    return true;
  }

  // TODO: expected code point
  cur_ptr_ = tmp;
  return false;
}

auto Lexer::tryScanRegexLiteral(const char* tok_start, bool must_be_regex,
                                bool& completely_erroneous) const -> const
    char* {
  bool is_forward_slash = tok_start[0] == '/';

  auto space_or_tab_description = [](char c) -> llvm::StringRef {
    switch (c) {
      case ' ':
        return "space";
      case '\t':
        return "tab";
      default:
        llvm_unreachable("Unhandled case");
    }
  };

  if (is_forward_slash) {
    const auto* regex_content_start = tok_start + 1;
    if (*regex_content_start == ' ' || *regex_content_start == '\t') {
      if (!must_be_regex) {
        return nullptr;
      }
    }
  }

  auto* ptr = tok_start;

  // TODO: Lex regex, not finish, just return nullptr.

  if (ptr == tok_start) {
    return nullptr;
  }

  assert(ptr > tok_start && ptr <= buffer_end_ && "Invalid pointer");
  return ptr;
}

auto Lexer::tryLexRegexLiteral(const char* tok_start) -> bool {
  bool is_forward_slash = tok_start[0] == '/';
  bool must_be_regex = true;

  if (is_forward_slash) {
    switch (forward_slash_regex_mode_) {
      case Radium::LexerForwardSlashRegexMode::None:
        return false;
      case Radium::LexerForwardSlashRegexMode::Tentative:
        must_be_regex = false;
        break;
      case Radium::LexerForwardSlashRegexMode::Always:
        break;
    }
  }

  bool completed_erroneous = false;
  auto* ptr =
      tryScanRegexLiteral(tok_start, must_be_regex, completed_erroneous);
  if (!ptr) {
    return false;
  }
  cur_ptr_ = ptr;
  if (completed_erroneous) {
    formToken(TokenKind::unknown, tok_start);
    return true;
  }

  formToken(TokenKind::regex_literal, tok_start);
  return true;
}

auto Lexer::tryLexConflictMarker(bool eat_newline) -> bool {
  // TODO
  return false;
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