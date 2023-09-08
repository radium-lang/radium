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

//===----------------------------------------------------------------------===//
// UTF8 Validation/Encoding/Decoding helper functions
//===----------------------------------------------------------------------===//

/// 将指定的码点编码为UTF8流。如果是错误的代码点，则返回true。
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
    result.push_back(char(0xF0 | (char_value >> (6 + 6 + 6))));
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

/// Given a pointer to the starting byte of a
/// UTF8 character, validate it and advance the lexer past it.  This returns the
/// encoded character or ~0U if the encoding is invalid.
static auto validateUTF8CharacterAndAdvance(const char*& ptr, const char* end)
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
  if (encoded_bytes == 1 ||
      // If the number of encoded bytes is > 4, then this is an invalid
      // character in the range of 0xF5 and above.  These would start an
      // encoding for something that couldn't be represented with UTF16
      // digraphs, so Unicode rejects them.
      encoded_bytes > 4) {
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

Lexer::Lexer(const SourceManager& src_mgr, bool in_ril_mode, unsigned buffer_id,
             bool keep_comments)
    : src_mgr_(src_mgr),
      buffer_id_(buffer_id),
      in_ril_mode_(in_ril_mode),
      keep_comments_(keep_comments) {
  const auto* buffer = src_mgr_->getMemoryBuffer(buffer_id);
  buffer_start_ = buffer->getBufferStart();
  buffer_end_ = buffer->getBufferEnd();
  cur_ptr_ = buffer_start_;

  if (buffer_id_ == src_mgr_.getCodeCompletionBufferID()) {
    const char* ptr = buffer_start_ + src_mgr_.getCodeCompletionOffset();
    if (ptr >= buffer_start_ && ptr <= buffer_end_) {
      code_completion_ptr_ = ptr;
    }
  }
}

void Lexer::primeLexer() {
  assert(next_token_.is(TokenKind::num_tokens));
  lexImpl();
  assert((next_token_.isAtStartOfLine() || cur_ptr_ != buffer_start_) &&
         "The token should be at the beginning of the line, "
         "or we should be lexing from the middle of the buffer");
}

void Lexer::initSubLexer(Lexer& parent, State begin_state, State end_state) {
  assert(buffer_id_ == src_mgr_.findBufferContainingLoc(begin_state.loc_) &&
         "state for the wrong buffer");
  assert(buffer_id_ == src_mgr_.findBufferContainingLoc(end_state.loc_) &&
         "state for the wrong buffer");

  // 如果父词法分析器应该提前停止，并且artificial_eof_位置在这个子范围内，
  // 那么我们也应该在该点停止。
  if (parent.artificial_eof_ && parent.artificial_eof_ >= buffer_start_ &&
      parent.artificial_eof_ <= buffer_end_) {
    artificial_eof_ = parent.artificial_eof_;
  } else {
    artificial_eof_ = getBufferPtrForSourceLoc(end_state.loc_);
  }

  primeLexer();
  restoreState(begin_state);
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
  // 当从文件缓冲区的中间对子范围进行lexing时，将会可能超过该范围的末尾，但仍会处于文件内。
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

}  // namespace Radium