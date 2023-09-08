#include "Radium/Basic/FileSystem.h"

#include <gtest/gtest.h>

#include "Radium/Basic/LLVM.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/raw_ostream.h"

#define ASSERT_NO_ERROR(x)                                                   \
  do                                                                         \
    if (std::error_code ASSERT_NO_ERROR_ec = x) {                            \
      llvm::errs() << #x ": did not return errc::success.\n"                 \
                   << "error number: " << ASSERT_NO_ERROR_ec.value() << "\n" \
                   << "error message: " << ASSERT_NO_ERROR_ec.message()      \
                   << "\n";                                                  \
      FAIL();                                                                \
    }                                                                        \
  while (0)

namespace Radium {
namespace {

using namespace llvm::sys;

TEST(FileSystem, MoveFileIfDifferentEmpty) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dir_path;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dir_path));

  // Test 1: Move empty over nonexistent.
  llvm::SmallString<128> source_file = dir_path;
  path::append(source_file, "source.txt");
  {
    std::error_code error;
    llvm::raw_fd_ostream empty_out(source_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
  }

  fs::UniqueID orig_id;
  ASSERT_NO_ERROR(fs::getUniqueID(source_file, orig_id));

  llvm::SmallString<128> dest_file = dir_path;
  path::append(dest_file, "dest.txt");
  ASSERT_FALSE(fs::exists(dest_file));

  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, dest_file));
  ASSERT_TRUE(fs::exists(dest_file));
  ASSERT_FALSE(fs::exists(source_file));

  fs::UniqueID dest_id;
  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, dest_id));
  ASSERT_EQ(orig_id, dest_id);

  ASSERT_NO_ERROR(fs::rename(dest_file, source_file));

  // Test 2: Move empty over empty.
  {
    std::error_code error;
    llvm::raw_fd_ostream empty_out(dest_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
  }

  fs::UniqueID new_id;
  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, new_id));
  ASSERT_NE(orig_id, new_id);

  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, dest_file));
  ASSERT_TRUE(fs::exists(dest_file));
  ASSERT_FALSE(fs::exists(source_file));

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, dest_id));
  ASSERT_EQ(new_id, dest_id);

  ASSERT_NO_ERROR(fs::rename(dest_file, source_file));
  orig_id = dest_id;

  // Test 3: Move empty over non-empty.
  {
    std::error_code error;
    llvm::raw_fd_ostream non_empty_out(dest_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
    non_empty_out << "a";
  }

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, new_id));
  ASSERT_NE(orig_id, new_id);

  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, dest_file));
  ASSERT_TRUE(fs::exists(dest_file));
  ASSERT_FALSE(fs::exists(source_file));

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, dest_id));
  ASSERT_EQ(orig_id, dest_id);

  ASSERT_NO_ERROR(fs::rename(dest_file, source_file));

  // Test 4: Move empty over self.
  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, source_file));
  ASSERT_TRUE(fs::exists(source_file));

  ASSERT_NO_ERROR(fs::getUniqueID(source_file, new_id));
  ASSERT_EQ(orig_id, new_id);

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(source_file, false));
  ASSERT_NO_ERROR(fs::remove(dir_path, false));
}

TEST(FileSystem, MoveFileIfDifferentNonEmpty) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dir_path;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dir_path));

  // Test 1: Move source over nonexistent.
  llvm::SmallString<128> source_file = dir_path;
  path::append(source_file, "source.txt");
  {
    std::error_code error;
    llvm::raw_fd_ostream source_out(source_file, error, fs::OF_None);
    source_out << "original";
    ASSERT_NO_ERROR(error);
  }

  fs::UniqueID orig_id;
  ASSERT_NO_ERROR(fs::getUniqueID(source_file, orig_id));

  llvm::SmallString<128> dest_file = dir_path;
  path::append(dest_file, "dest.txt");
  ASSERT_FALSE(fs::exists(dest_file));

  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, dest_file));
  ASSERT_TRUE(fs::exists(dest_file));
  ASSERT_FALSE(fs::exists(source_file));

  fs::UniqueID dest_id;
  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, dest_id));
  ASSERT_EQ(orig_id, dest_id);

  ASSERT_NO_ERROR(fs::rename(dest_file, source_file));

  // Test 2: Move source over empty.
  {
    std::error_code error;
    llvm::raw_fd_ostream empty_out(dest_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
  }

  fs::UniqueID new_id;
  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, new_id));
  ASSERT_NE(orig_id, new_id);

  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, dest_file));
  ASSERT_TRUE(fs::exists(dest_file));
  ASSERT_FALSE(fs::exists(source_file));

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, dest_id));
  ASSERT_EQ(orig_id, dest_id);

  ASSERT_NO_ERROR(fs::rename(dest_file, source_file));

  // Test 3: Move source over non-empty-but-different.
  {
    std::error_code error;
    llvm::raw_fd_ostream non_empty_out(dest_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
    non_empty_out << "different";
  }

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, new_id));
  ASSERT_NE(orig_id, new_id);

  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, dest_file));
  ASSERT_TRUE(fs::exists(dest_file));
  ASSERT_FALSE(fs::exists(source_file));

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, dest_id));
  ASSERT_EQ(orig_id, dest_id);

  ASSERT_NO_ERROR(fs::rename(dest_file, source_file));

  // Test 4: Move source over identical.
  {
    std::error_code error;
    llvm::raw_fd_ostream non_empty_out(dest_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
    non_empty_out << "original";
  }

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, new_id));
  ASSERT_NE(orig_id, new_id);

  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, dest_file));
  ASSERT_TRUE(fs::exists(dest_file));
  ASSERT_FALSE(fs::exists(source_file));

  ASSERT_NO_ERROR(fs::getUniqueID(dest_file, dest_id));
  ASSERT_EQ(new_id, dest_id);

  ASSERT_NO_ERROR(fs::rename(dest_file, source_file));
  orig_id = new_id;

  // Test 5: Move source over self.
  ASSERT_NO_ERROR(moveFileIfDifferent(source_file, source_file));
  ASSERT_TRUE(fs::exists(source_file));

  ASSERT_NO_ERROR(fs::getUniqueID(source_file, new_id));
  ASSERT_EQ(orig_id, new_id);

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(source_file, false));
  ASSERT_NO_ERROR(fs::remove(dir_path, false));
}

TEST(FileSystem, MoveFileIfDifferentNonExistent) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dir_path;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dir_path));

  llvm::SmallString<128> source_file = dir_path;
  path::append(source_file, "source.txt");
  llvm::SmallString<128> dest_file = dir_path;
  path::append(dest_file, "dest.txt");

  // Test 1: Nonexistent -> nonexistent.
  ASSERT_TRUE((bool)moveFileIfDifferent(source_file, dest_file));

  {
    std::error_code error;
    llvm::raw_fd_ostream empty_out(dest_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
  }

  // Test 2: Nonexistent -> present.
  ASSERT_TRUE((bool)moveFileIfDifferent(source_file, dest_file));

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(dest_file));
  ASSERT_NO_ERROR(fs::remove(dir_path));
}

TEST(FileSystem, MoveFileIfDifferentInvalid) {
  // Create unique temporary directory for these tests
  llvm::SmallString<128> dir_path;
  ASSERT_NO_ERROR(fs::createUniqueDirectory("FileSystem-test", dir_path));

  llvm::SmallString<128> source_file = dir_path;
  path::append(source_file, "source.txt");
  {
    std::error_code error;
    llvm::raw_fd_ostream empty_out(source_file, error, fs::OF_None);
    ASSERT_NO_ERROR(error);
  }

  // Test 1: Move a file over its parent directory.
  ASSERT_TRUE((bool)moveFileIfDifferent(source_file, dir_path));

  // Test 2: Move a file into a nonexistent directory.
  llvm::SmallString<128> dest_file = dir_path;
  path::append(dest_file, "nonexistent", "dest.txt");
  ASSERT_TRUE((bool)moveFileIfDifferent(source_file, dest_file));

  // Clean up.
  ASSERT_NO_ERROR(fs::remove(source_file, false));
  ASSERT_NO_ERROR(fs::remove(dir_path, false));
}

}  // namespace
}  // namespace Radium