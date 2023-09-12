#include "Radium/Parse/Lexer.h"

#include "Radium/AST/Identifier.h"
#include "Radium/Basic/Fallthrough.h"
#include "Radium/Basic/SourceManager.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/MathExtras.h"
#include "llvm/Support/MemoryBuffer.h"

namespace Radium {

// //===----------------------------------------------------------------------===//
// // UTF8 Validation/Encoding/Decoding helper functions
// //===----------------------------------------------------------------------===//
//
// /// 将指定的码点编码为UTF8流。如果是错误的代码点，则返回true。
// static auto EncodeToUTF8(unsigned char_value, SmallVectorImpl<char>& result)
//     -> bool {
//   // Number of bits in the value, ignoring leading zeros.
//   unsigned num_bits = 32 - llvm::countLeadingZeros(char_value);
//
//   // Handle the leading byte, based on the number of bits in the value.
//   unsigned num_trailing_bytes;
//   if (num_bits <= 5 + 6) {
//     // Encoding is 0x110aaaaa 10bbbbbb
//     result.push_back(static_cast<char>(0xC0 | (char_value >> 6)));
//     num_trailing_bytes = 1;
//   } else if (num_bits <= 4 + 6 + 6) {
//     // Encoding is 0x1110aaaa 10bbbbbb 10cccccc
//     result.push_back(static_cast<char>(0xE0 | (char_value >> (6 + 6))));
//     num_trailing_bytes = 2;
//
//     // UTF-16 surrogate pair values are not valid code points.
//     if (char_value >= 0xD800 && char_value <= 0xDFFF) {
//       return true;
//     }
//     // U+FDD0...U+FDEF are also reserved
//     if (char_value >= 0xFDD0 && char_value <= 0xFDEF) {
//       return true;
//     }
//   } else if (num_bits <= 3 + 6 + 6 + 6) {
//     // Encoding is 0x11110aaa 10bbbbbb 10cccccc 10dddddd
//     result.push_back(char(0xF0 | (char_value >> (6 + 6 + 6))));
//     num_trailing_bytes = 3;
//     // Reject over-large code points.  These cannot be encoded as UTF-16
//     // surrogate pairs, so UTF-32 doesn't allow them.
//     if (char_value > 0x10FFFF) {
//       return true;
//     }
//   } else {
//     return true;  // UTF8 can encode these, but they aren't valid code
//     points.
//   }
//
//   // Emit all of the trailing bytes.
//   while (num_trailing_bytes--) {
//     result.push_back(static_cast<char>(
//         0x80 | (0x3F & (char_value >> (num_trailing_bytes * 6)))));
//   }
//   return false;
// }
//
// /// CLO8 - Return the number of leading ones in the specified 8-bit value.
// static auto CLO8(unsigned char c) -> unsigned {
//   return llvm::countLeadingOnes(static_cast<uint32_t>(c) << 24);
// }
//
// /// isStartOfUTF8Character - Return true if this isn't a UTF8 continuation
// /// character, which will be of the form 0b10XXXXXX
// static auto isStartOfUTF8Character(unsigned char c) -> bool {
//   // RFC 2279: The octet values FE and FF never appear.
//   // RFC 3629: The octet values C0, C1, F5 to FF never appear.
//   return c <= 0x80 || (c >= 0xC2 && c < 0xF5);
// }
//
// /// Given a pointer to the starting byte of a
// /// UTF8 character, validate it and advance the lexer past it.  This returns
// the
// /// encoded character or ~0U if the encoding is invalid.
// static auto validateUTF8CharacterAndAdvance(const char*& ptr, const char*
// end)
//     -> uint32_t {
//   if (ptr >= end) {
//     return ~0U;
//   }
//
//   unsigned char cur_byte = *ptr++;
//   if (cur_byte < 0x80) {
//     return cur_byte;
//   }
//
//   // Read the number of high bits set, which indicates the number of bytes in
//   // the character.
//   unsigned encoded_bytes = CLO8(cur_byte);
//
//   // If this is 0b10XXXXXX, then it is a continuation character.
//   if (encoded_bytes == 1 ||
//       // If the number of encoded bytes is > 4, then this is an invalid
//       // character in the range of 0xF5 and above.  These would start an
//       // encoding for something that couldn't be represented with UTF16
//       // digraphs, so Unicode rejects them.
//       encoded_bytes > 4) {
//     // Skip until we get the start of another character.  This is guaranteed
//     to
//     // at least stop at the nul at the end of the buffer.
//     while (ptr < end && !isStartOfUTF8Character(*ptr)) {
//       ++ptr;
//     }
//     return ~0U;
//   }
//
//   // Drop the high bits indicating the # bytes of the result.
//   unsigned char_value =
//       static_cast<unsigned char>(cur_byte << encoded_bytes) >> encoded_bytes;
//
//   // Read and validate the continuation bytes.
//   for (unsigned i = 1; i != encoded_bytes; ++i) {
//     if (ptr >= end) {
//       return ~0U;
//     }
//     cur_byte = *ptr;
//     // If the high bit isn't set or the second bit isn't clear, then this is
//     not
//     // a continuation byte!
//     if (cur_byte < 0x80 || cur_byte >= 0xC0) {
//       return ~0U;
//     }
//
//     // Accumulate our result.
//     char_value <<= 6;
//     char_value |= cur_byte & 0x3F;
//     ++ptr;
//   }
//
//   // UTF-16 surrogate pair values are not valid code points.
//   if (char_value >= 0xD800 && char_value <= 0xDFFF) {
//     return ~0U;
//   }
//
//   // If we got here, we read the appropriate number of accumulated bytes.
//   // Verify that the encoding was actually minimal.
//   // Number of bits in the value, ignoring leading zeros.
//   unsigned num_bits = 32 - llvm::countLeadingZeros(char_value);
//
//   if (num_bits <= 5 + 6) {
//     return encoded_bytes == 2 ? char_value : ~0U;
//   }
//   if (num_bits <= 4 + 6 + 6) {
//     return encoded_bytes == 3 ? char_value : ~0U;
//   }
//   return encoded_bytes == 4 ? char_value : ~0U;
// }

Lexer::Lexer(const SourceManager& src_mgr, unsigned buffer_id, bool in_ril_mode,
             bool keep_comments)
    : src_mgr_(src_mgr),
      buffer_id_(buffer_id),
      in_ril_mode_(in_ril_mode),
      keep_comments_(keep_comments) {}

Lexer::Lexer(unsigned buffer_id, const SourceManager& src_mgr, bool in_ril_mode,
             bool keep_comments)
    : Lexer(src_mgr, buffer_id, in_ril_mode, keep_comments) {
  unsigned end_offset = src_mgr_.getRangeForBuffer(buffer_id_).getByteLength();
  initialize(0, end_offset);
}

Lexer::Lexer(Lexer& parent, State begin_state, State end_state,
             bool in_ril_mode, bool keep_comments)
    : Lexer(parent.src_mgr_, parent.buffer_id_, in_ril_mode, keep_comments) {
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
  Lexer lexer(src_mgr_, in_ril_mode_, buffer_id_, /*keep_comments_=*/false);
  lexer.restoreState(State(loc));
  Token result;
  lexer.lex(result);
  return result;
}

void Lexer::formToken(TokenKind kind, const char* tok_start) {
  // 当从文件缓冲区的中间对子范围进行lexing时，将会可能超过该范围的末尾，
  // 但仍会处于文件内。
  // 这里检查是否超过了人工EOF，超过则合成一个TokenKind::eof。
  if (kind != TokenKind::eof && artificial_eof_ &&
      tok_start >= artificial_eof_) {
    kind = TokenKind::eof;
  }
  next_token_.setToken(kind, StringRef(tok_start, cur_ptr_ - tok_start));
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

void Lexer::skipToEndOfLine() {
  while (true) {
    switch (*cur_ptr_++) {
      case '\n':
      case '\r':
        next_token_.setAtStartOfLine(true);
        return;
      default:
        break;
    }
  }
}

void Lexer::skipSlashSlashComment() {
  assert(cur_ptr_[-1] == '/' && cur_ptr_[0] == '/' && "Not a // comment");
  skipToEndOfLine();
}

void Lexer::skipHashbang() {
  assert(cur_ptr_ == buffer_start_ && cur_ptr_[0] == '#' &&
         cur_ptr_[1] == '!' && "Not a hashbang");
  skipToEndOfLine();
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

static bool isValidIdentifierContinuationCodePoint(uint32_t c) {
  if (c < 0x80)
    return isalnum(c) || c == '_' || c == '$';

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
         (c >= 0xFDF0 && c <= 0xFE44) || (c >= 0xFE47 && c <= 0xFFFD)

         || (c >= 0x10000 && c <= 0x1FFFD) || (c >= 0x20000 && c <= 0x2FFFD) ||
         (c >= 0x30000 && c <= 0x3FFFD) || (c >= 0x40000 && c <= 0x4FFFD) ||
         (c >= 0x50000 && c <= 0x5FFFD) || (c >= 0x60000 && c <= 0x6FFFD) ||
         (c >= 0x70000 && c <= 0x7FFFD) || (c >= 0x80000 && c <= 0x8FFFD) ||
         (c >= 0x90000 && c <= 0x9FFFD) || (c >= 0xA0000 && c <= 0xAFFFD) ||
         (c >= 0xB0000 && c <= 0xBFFFD) || (c >= 0xC0000 && c <= 0xCFFFD) ||
         (c >= 0xD0000 && c <= 0xDFFFD) || (c >= 0xE0000 && c <= 0xEFFFD);
}
static bool isValidIdentifierStartCodePoint(uint32_t c) {
  if (!isValidIdentifierContinuationCodePoint(c))
    return false;
  if (c < 0x80 && (isnumber(c) || c == '$'))
    return false;

  // N1518: Recommendations for extended identifier characters for C and C++
  // Proposed Annex X.2: Ranges of characters disallowed initially
  if ((c >= 0x0300 && c <= 0x036F) || (c >= 0x1DC0 && c <= 0x1DFF) ||
      (c >= 0x20D0 && c <= 0x20FF) || (c >= 0xFE20 && c <= 0xFE2F))
    return false;

  return true;
}

static bool advanceIf(char const*& ptr, char const* end,
                      bool (*predicate)(uint32_t)) {
  char const* next = ptr;
  uint32_t c = validateUTF8CharacterAndAdvance(next, end);
  if (c == ~0U)
    return false;
  if (predicate(c)) {
    ptr = next;
    return true;
  }
  return false;
}

static bool advanceIfValidStartOfIdentifier(char const*& ptr, char const* end) {
  return advanceIf(ptr, end, isValidIdentifierStartCodePoint);
}

static bool advanceIfValidContinuationOfIdentifier(char const*& ptr,
                                                   char const* end) {
  return advanceIf(ptr, end, isValidIdentifierContinuationCodePoint);
}

static bool advanceIfValidStartOfOperator(char const*& ptr, char const* end) {
  return advanceIf(ptr, end, Identifier::isOperatorStartCodePoint);
}

static bool advanceIfValidContinuationOfOperator(char const*& ptr,
                                                 char const* end) {
  return advanceIf(ptr, end, Identifier::isOperatorContinuationCodePoint);
}

/// isIdentifier - Checks whether a string matches the identifier regex.
bool Lexer::isIdentifier(llvm::StringRef string) {
  if (string.empty())
    return false;
  char const *p = string.data(), *end = string.end();
  if (!advanceIfValidStartOfIdentifier(p, end))
    return false;
  while (p < end && advanceIfValidContinuationOfIdentifier(p, end))
    ;
  return p == end;
}

/// lexIdentifier - Match [a-zA-Z_][a-zA-Z_$0-9]*
///
/// FIXME: We should also allow unicode characters in identifiers.
void Lexer::lexIdentifier() {
  const char* TokStart = CurPtr - 1;
  CurPtr = TokStart;
  bool didStart = advanceIfValidStartOfIdentifier(CurPtr, BufferEnd);
  assert(didStart && "Unexpected start");
  (void)didStart;

  // Lex [a-zA-Z_$0-9[[:XID_Continue:]]]*
  while (advanceIfValidContinuationOfIdentifier(CurPtr, BufferEnd))
    ;

  tok Kind = llvm::StringSwitch<tok>(StringRef(TokStart, CurPtr - TokStart))
#define KEYWORD(kw) .Case(#kw, tok::kw_##kw)
#include "swift/Parse/Tokens.def"
                 .Default(tok::identifier);

  // "sil" and "sil_stage" are only keywords in SIL mode.
  if ((Kind == tok::kw_sil || Kind == tok::kw_sil_stage) && !InSILMode)
    Kind = tok::identifier;

  return formToken(Kind, TokStart);
}

void Lexer::lexImpl() {
  assert(cur_ptr_ >= buffer_start_ && cur_ptr_ <= buffer_end_ &&
         "Current pointer out of range!");

  next_token_.setAtStartOfLine(cur_ptr_ == buffer_start_);

  const char* token_start = cur_ptr_;
  switch (*cur_ptr_++) {
    case '\n':
    case '\r':
      next_token_.setAtStartOfLine(true);
      return formToken(TokenKind::new_line, token_start);

    case ' ':
    case '\t':
    case '\f':
    case '\v':
      return formToken(TokenKind::whitespace, token_start);

    case '{':
      return formToken(TokenKind::l_brace, token_start);
    case '}':
      return formToken(TokenKind::r_brace, token_start);
    case '(':
      return formToken(TokenKind::l_paren, token_start);
    case ')':
      return formToken(TokenKind::r_paren, token_start);
    case '[':
      return formToken(TokenKind::l_square, token_start);
    case ']':
      return formToken(TokenKind::r_square, token_start);
    case ',':
      return formToken(TokenKind::comma, token_start);
    case ';':
      return formToken(TokenKind::semi, token_start);
    case ':':
      return formToken(TokenKind::colon, token_start);
    case '?':
      return formToken(TokenKind::question_postfix, token_start);

    case '=':
    case '-':
    case '+':
    case '*':
    case '<':
    case '>':
    case '&':
    case '|':
    case '^':
    case '~':
      return lexOperatorIdentifier();

    case '.':
      return lexOperatorIdentifier();

    case 'A':
    case 'B':
    case 'C':
    case 'D':
    case 'E':
    case 'F':
    case 'G':
    case 'H':
    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
    case 'V':
    case 'W':
    case 'X':
    case 'Y':
    case 'Z':
    case 'a':
    case 'b':
    case 'c':
    case 'd':
    case 'e':
    case 'f':
    case 'g':
    case 'h':
    case 'i':
    case 'j':
    case 'k':
    case 'l':
    case 'm':
    case 'n':
    case 'o':
    case 'p':
    case 'q':
    case 'r':
    case 's':
    case 't':
    case 'u':
    case 'v':
    case 'w':
    case 'x':
    case 'y':
    case 'z':
    case '_':
      return lexIdentifier();

    case '$':
      return lexDollarIdent();

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      return lexNumber();

    case '\'':
      return lexCharacterLiteral();
    case '"':
      return lexStringLiteral();
  }
}

}  // namespace Radium