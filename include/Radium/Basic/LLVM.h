#ifndef RADIUM_BASIC_LLVM_H
#define RADIUM_BASIC_LLVM_H

#include "llvm/Support/Casting.h"

namespace llvm {
// Containers
class StringRef;
class StringLiteral;
class Twine;
template <typename T>
class SmallPtrSetImpl;
template <typename T, unsigned N>
class SmallPtrSet;
template <typename T>
class SmallVectorImpl;
template <typename T, unsigned N>
class SmallVector;
template <unsigned N>
class SmallString;
template <typename T, unsigned N>
class SmallSetVector;
template <typename T>
class ArrayRef;
template <typename T>
class MutableArrayRef;
template <typename T>
class TinyPtrVector;
template <typename... PTs>
class PointerUnion;
template <typename IteratorT>
class iterator_range;
class SmallBitVector;

// Other common classes.
class raw_ostream;
class APInt;
class APFloat;
template <typename Fn>
class function_ref;

}  // namespace llvm

namespace Radium {

// Casting operators.
using llvm::cast;
using llvm::cast_or_null;
using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::isa;
using llvm::isa_and_nonnull;

// Containers
using llvm::ArrayRef;
using llvm::iterator_range;
using llvm::MutableArrayRef;
using llvm::PointerUnion;
using llvm::SmallBitVector;
using llvm::SmallPtrSet;
using llvm::SmallPtrSetImpl;
using llvm::SmallSetVector;
using llvm::SmallString;
using llvm::SmallVector;
using llvm::SmallVectorImpl;
using llvm::StringLiteral;
using llvm::StringRef;
using llvm::TinyPtrVector;
using llvm::Twine;

// Other common classes.
using llvm::APFloat;
using llvm::APInt;
using llvm::function_ref;
using llvm::raw_ostream;

}  // namespace Radium

#endif  // RADIUM_BASIC_LLVM_H