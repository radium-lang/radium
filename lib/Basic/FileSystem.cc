#include "Radium/Basic/FileSystem.h"

#include "clang/Basic/FileManager.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/Errc.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Process.h"
#include "llvm/Support/Signals.h"
#include "llvm/Support/VirtualFileSystem.h"

namespace Radium {

namespace {
class OpenFileRAII {
  static const int INVALID_FD = -1;

 public:
  int fd = INVALID_FD;

  ~OpenFileRAII() {
    if (fd != INVALID_FD) {
      llvm::sys::Process::SafelyCloseFileDescriptor(fd);
    }
  }
};
}  // anonymous namespace

/// 作为一个简单的验证，检查是否可以在 \p output_path 旁边写入临时文件，
static auto canUseTemporaryForWrite(const llvm::StringRef output_path)
    -> llvm::ErrorOr<bool> {
  namespace fs = llvm::sys::fs;

  if (output_path == "-") {
    // Special case: "-" represents stdout, and LLVM's output stream APIs are
    // aware of this. It doesn't make sense to use a temporary in this case.
    return false;
  }

  fs::file_status status;
  (void)fs::status(output_path, status);
  if (!fs::exists(status)) {
    // Assume we'll be able to write to both a temporary file and to the final
    // destination if the final destination doesn't exist yet.
    return true;
  }

  // Fail early if we can't write to the final destination.
  if (!fs::can_write(output_path)) {
    return llvm::make_error_code(llvm::errc::operation_not_permitted);
  }

  // Only use a temporary if the output is a regular file. This handles
  // things like '-o /dev/null'
  return fs::is_regular_file(status);
}

/// 尝试在 \p output_path 旁边打开一个临时文件，一旦文件被写入，该临时文件将
/// 被重命名。
static auto tryToOpenTemporaryFile(
    llvm::Optional<llvm::raw_fd_ostream>& opened_stream,
    const llvm::StringRef output_path) -> llvm::Optional<std::string> {
  namespace fs = llvm::sys::fs;

  // Create a temporary file path.
  // Insert a placeholder for a random suffix before the extension (if any).
  // Then because some tools glob for build artifacts (such as clang's own
  // GlobalModuleIndex.cpp), also append .tmp.
  llvm::SmallString<128> temp_path;
  const llvm::StringRef output_extension =
      llvm::sys::path::extension(output_path);
  temp_path = output_path.drop_back(output_extension.size());
  temp_path += "-%%%%%%%%";
  temp_path += output_extension;
  temp_path += ".tmp";

  int fd;
  const unsigned perms = fs::all_read | fs::all_write;
  std::error_code ec =
      fs::createUniqueFile(temp_path, fd, temp_path, fs::OF_None, perms);

  if (ec) {
    // Ignore the specific error; the caller has to fall back to not using a
    // temporary anyway.
    return llvm::None;
  }

  opened_stream.emplace(fd, /*shouldClose=*/true);
  // Make sure the temporary file gets removed if we crash.
  llvm::sys::RemoveFileOnSignal(temp_path);
  return temp_path.str().str();
}

auto atomicallyWritingToFile(
    const llvm::StringRef output_path,
    const llvm::function_ref<void(llvm::raw_pwrite_stream&)> action)
    -> std::error_code {
  namespace fs = llvm::sys::fs;

  // FIXME: This is mostly a simplified version of
  // clang::CompilerInstance::createOutputFile. It would be great to share the
  // implementation.
  assert(!output_path.empty());

  llvm::ErrorOr<bool> can_use_temporary = canUseTemporaryForWrite(output_path);
  if (std::error_code error = can_use_temporary.getError()) {
    return error;
  }

  llvm::Optional<std::string> temporary_path;
  {
    llvm::Optional<llvm::raw_fd_ostream> os;
    if (can_use_temporary.get()) {
      temporary_path = tryToOpenTemporaryFile(os, output_path);

      if (!temporary_path) {
        assert(!os.has_value());
        // If we failed to create the temporary, fall back to writing to the
        // file directly. This handles the corner case where we cannot write to
        // the directory, but can write to the file.
      }
    }

    if (!os.has_value()) {
      std::error_code error;
      os.emplace(output_path, error, fs::OF_None);
      if (error) {
        return error;
      }
    }

    action(os.value());
    // In addition to scoping the use of 'OS', ending the scope here also
    // ensures that it's been flushed (by destroying it).
  }

  if (!temporary_path.has_value()) {
    // If we didn't use a temporary, we're done!
    return std::error_code();
  }

  return moveFileIfDifferent(temporary_path.value(), output_path);
}

auto areFilesDifferent(const llvm::Twine& source,
                       const llvm::Twine& destination,
                       bool allow_destination_errors)
    -> llvm::ErrorOr<FileDifference> {
  namespace fs = llvm::sys::fs;

  if (fs::equivalent(source, destination)) {
    return FileDifference::IdenticalFile;
  }

  OpenFileRAII source_file;
  fs::file_status source_status;
  if (std::error_code error = fs::openFileForRead(source, source_file.fd)) {
    // If we can't open the source file, fail.
    return error;
  }
  if (std::error_code error = fs::status(source_file.fd, source_status)) {
    // If we can't stat the source file, fail.
    return error;
  }

  /// Converts an error from the destination file into either an error or
  /// DifferentContents return, depending on `allowDestinationErrors`.
  auto convert_destination_error =
      [allow_destination_errors](
          std::error_code error) -> llvm::ErrorOr<FileDifference> {
    if (allow_destination_errors) {
      return FileDifference::DifferentContents;
    }
    return error;
  };

  OpenFileRAII dest_file;
  fs::file_status dest_status;
  if (std::error_code error = fs::openFileForRead(destination, dest_file.fd)) {
    // If we can't open the destination file, fail in the specified fashion.
    return convert_destination_error(error);
  }
  if (std::error_code error = fs::status(dest_file.fd, dest_status)) {
    // If we can't open the destination file, fail in the specified fashion.
    return convert_destination_error(error);
  }

  uint64_t size = source_status.getSize();
  if (size != dest_status.getSize()) {
    // If the files are different sizes, they must be different.
    return FileDifference::DifferentContents;
  }
  if (size == 0) {
    // If both files are zero size, they must be the same.
    return FileDifference::SameContents;
  }

  // The two files match in size, so we have to compare the bytes to determine
  // if they're the same.
  std::error_code source_region_err;
  fs::mapped_file_region source_region(
      fs::convertFDToNativeFile(source_file.fd),
      fs::mapped_file_region::readonly, size, 0, source_region_err);
  if (source_region_err) {
    return source_region_err;
  }

  std::error_code dest_region_err;
  fs::mapped_file_region dest_region(fs::convertFDToNativeFile(dest_file.fd),
                                     fs::mapped_file_region::readonly, size, 0,
                                     dest_region_err);

  if (dest_region_err) {
    return convert_destination_error(dest_region_err);
  }

  if (memcmp(source_region.const_data(), dest_region.const_data(), size) != 0) {
    return FileDifference::DifferentContents;
  }

  return FileDifference::SameContents;
}

auto moveFileIfDifferent(const llvm::Twine& source,
                         const llvm::Twine& destination) -> std::error_code {
  namespace fs = llvm::sys::fs;

  auto result = areFilesDifferent(source, destination,
                                  /*allow_destination_errors=*/true);

  if (!result) {
    return result.getError();
  }

  switch (*result) {
    case FileDifference::IdenticalFile:
      // Do nothing for a self-move.
      return std::error_code();

    case FileDifference::SameContents:
      // Files are identical; remove the source file.
      return fs::remove(source);

    case FileDifference::DifferentContents:
      // Files are different; overwrite the destination file.
      return fs::rename(source, destination);
  }
  llvm_unreachable("Unhandled FileDifference in switch");
}

auto vfs::getFileOrSTDIN(llvm::vfs::FileSystem& fs, const llvm::Twine& filename,
                         int64_t file_size, bool requires_null_terminator,
                         bool is_volatile, unsigned badf_retry)
    -> llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> {
  llvm::SmallString<256> name_buf;
  llvm::StringRef name_ref = filename.toStringRef(name_buf);

  if (name_ref == "-") {
    return llvm::MemoryBuffer::getSTDIN();
  }
  llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> input_file_or_err =
      nullptr;
  for (unsigned i = 0; i != badf_retry + 1; ++i) {
    input_file_or_err = fs.getBufferForFile(
        filename, file_size, requires_null_terminator, is_volatile);
    if (input_file_or_err) {
      return input_file_or_err;
    }
    if (input_file_or_err.getError().value() != EBADF) {
      return input_file_or_err;
    }
  }
  return input_file_or_err;
}

}  // namespace Radium