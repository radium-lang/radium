#include "Radium/Basic/SourceManager.h"

#include <gtest/gtest.h>

#include <vector>

#include "llvm/Support/MemoryBuffer.h"

namespace Radium {
namespace {

static auto tokenize(SourceManager& sm, llvm::StringRef src)
    -> std::vector<SourceLoc> {
  unsigned id = sm.addMemBufferCopy(src);
  const llvm::MemoryBuffer* buffer = sm.getLLVMSourceMgr().getMemoryBuffer(id);

  SourceLoc begin_loc(llvm::SMLoc::getFromPointer(buffer->getBuffer().begin()));
  std::vector<SourceLoc> result;
  result.push_back(begin_loc);
  for (unsigned i = 1, e = src.size(); i != e; ++i) {
    if (src[i - 1] == ' ') {
      result.push_back(begin_loc.getAdvancedLoc(i));
    }
  }
  return result;
}

TEST(SourceManager, isBeforeInBuffer) {
  SourceManager sm;
  auto locs = tokenize(sm, "aaa bbb ccc ddd");

  EXPECT_TRUE(sm.isBeforeInBuffer(locs[0], locs[1]));
  EXPECT_TRUE(sm.isBeforeInBuffer(locs[1], locs[2]));
  EXPECT_TRUE(sm.isBeforeInBuffer(locs[2], locs[3]));
  EXPECT_TRUE(sm.isBeforeInBuffer(locs[0], locs[3]));

  EXPECT_TRUE(sm.isBeforeInBuffer(locs[0], locs[0].getAdvancedLoc(1)));
  EXPECT_TRUE(sm.isBeforeInBuffer(locs[0].getAdvancedLoc(1), locs[1]));
}

TEST(SourceManager, RangeContainsTokenLoc) {
  SourceManager sm;
  auto locs = tokenize(sm, "aaa bbb ccc ddd");

  SourceRange r_aa(locs[0], locs[0]);
  SourceRange r_ab(locs[0], locs[1]);
  SourceRange r_ac(locs[0], locs[2]);

  SourceRange r_bc(locs[1], locs[2]);

  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_aa, locs[0]));
  EXPECT_FALSE(sm.rangeContainsTokenLoc(r_aa, locs[0].getAdvancedLoc(1)));

  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ab, locs[0]));
  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ab, locs[0].getAdvancedLoc(1)));
  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ab, locs[1]));
  EXPECT_FALSE(sm.rangeContainsTokenLoc(r_ab, locs[1].getAdvancedLoc(1)));

  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ac, locs[0]));
  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ac, locs[0].getAdvancedLoc(1)));
  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ac, locs[1]));
  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ac, locs[1].getAdvancedLoc(1)));
  EXPECT_TRUE(sm.rangeContainsTokenLoc(r_ac, locs[2]));
  EXPECT_FALSE(sm.rangeContainsTokenLoc(r_ac, locs[2].getAdvancedLoc(1)));

  EXPECT_FALSE(sm.rangeContainsTokenLoc(r_bc, locs[0]));
  EXPECT_FALSE(sm.rangeContainsTokenLoc(r_bc, locs[0].getAdvancedLoc(1)));
}

}  // namespace
}  // namespace Radium