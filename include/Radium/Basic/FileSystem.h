#ifndef RADIUM_BASIC_FILESYSTEM_H
#define RADIUM_BASIC_FILESYSTEM_H

#include <system_error>

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h"

namespace llvm {
class raw_pwrite_stream;
class Twine;

namespace vfs {
class FileSystem;
}  // namespace vfs
}  // namespace llvm

namespace Radium {

/// 原子性的写入文件。
/// 其中 \p action是一系列写raw_ostream到临时文件的操作。
/// \p outputPath是最终的输出文件。
auto atomicallyWritingToFile(
    llvm::StringRef output_path,
    llvm::function_ref<void(llvm::raw_pwrite_stream&)> action)
    -> std::error_code;

/// 将文件从 \p source 移动到 \p destination，除非 \p destination 已经存在
/// 并且数据和 \p source 相同。
auto moveFileIfDifferent(const llvm::Twine& source,
                         const llvm::Twine& destination) -> std::error_code;

enum class FileDifference : uint8_t {
  /// 源路径和目标路径引用完全相同的文件。
  IdenticalFile,
  /// 源路径和目标路径引用具有相同内容的单独文件。
  SameContents,
  /// 源路径和目标路径引用具有不同内容的单独文件。
  DifferentContents
};

/// 比较 \p source 和 \p destination 的文件，以确定它们是否是完全相同的文件，
/// 或者具有相同内容的不同文件，或者具有不同内容的不同文件。
/// \p allow_destination_errors 设置为true时，与 \p destination 文件相关的
/// 文件系统错误返回 \c FileDifference结果，而不是错误。
auto areFilesDifferent(const llvm::Twine& source,
                       const llvm::Twine& destination,
                       bool allow_destination_errors)
    -> llvm::ErrorOr<FileDifference>;

namespace vfs {
auto getFileOrSTDIN(llvm::vfs::FileSystem& fs, const llvm::Twine& name,
                    int64_t file_size = -1,
                    bool requires_null_terminator = true,
                    bool is_volatile = false, unsigned badf_retry = 0)
    -> llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>>;
}  // end namespace vfs
}  // namespace Radium

#endif  // RADIUM_BASIC_FILESYSTEM_H