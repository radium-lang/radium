#ifndef RADIUM_BASIC_LLVM_H
#define RADIUM_BASIC_LLVM_H

#include "llvm/Support/Casting.h"

namespace llvm {
// Containers
class StringRef;
class Twine;
template <typename T>
class SmallVectorImpl;
template <typename T, unsigned N>
class SmallVector;
template <typename T>
class ArrayRef;
template <typename T>
class MutableArrayRef;
template <typename T>
class TinyPtrVector;
template <typename PT1, typename PT2>
class PointerUnion;

// Other common classes.
class raw_ostream;
class APInt;
class APFloat;

}  // namespace llvm

namespace Radium {

// Casting operators.
using llvm::cast;
using llvm::cast_or_null;
using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::isa;

// Containers
using llvm::ArrayRef;
using llvm::MutableArrayRef;
using llvm::PointerUnion;
using llvm::SmallVector;
using llvm::SmallVectorImpl;
using llvm::StringRef;
using llvm::TinyPtrVector;
using llvm::Twine;

// Other common classes.
using llvm::APFloat;
using llvm::APInt;
using llvm::raw_ostream;

}  // namespace Radium

#endif  // RADIUM_BASIC_LLVM_H