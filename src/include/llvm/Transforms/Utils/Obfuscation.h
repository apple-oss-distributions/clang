//===-- Obfuscation.h - String obfuscation helper ---------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file provides helper classes to perform string obfuscation
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_OBFUSCATION_H
#define LLVM_TRANSFORMS_UTILS_OBFUSCATION_H

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MathExtras.h"

namespace llvm {
class Module;

namespace obfuscate {

/// \brief Base class providing basic obfuscation capability
///
/// Derived classes are to implement obfuscateImpl
class Obfuscator {
  using MapTy = StringMap<StringRef, BumpPtrAllocator &>;

  BumpPtrAllocator Alloc;

  // Whether we want to remember the reverse mapping
  bool Reverse = false;

  // Track the mapping, to enforce idempotence under sequential composition
  MapTy ForwardMap;

  // For when we want to track a reverse mapping
  MapTy ReverseMap;

  // Track the type name mapping, forwards only
  MapTy TyForwardMap;

protected:
  // Derived classes will implement this method to return the newly obfuscated
  // string. Derived classe are free to track internal state, but are not
  // required to (e.g. the base class tracks the reverse mapping, forward
  // mapping, and provides an allocator).
  //
  // The base class will only ever call this once for any unique input,
  // enforcing idempotence under sequential composition.
  virtual StringRef obfuscateImpl(StringRef) = 0;

  // Derived classes may implement this method to return an obfuscated type
  // name. Type names are special in that they don't need even be kept, but they
  // help libLTO's heuristic and can keep code size down. Thus, we will
  // obfuscate them, and want to obfuscate them consistently (they will reside
  // in the ForwardsMap), but we will never care to reverse them.
  virtual StringRef obfuscateTypeNameImpl(StringRef S) {
    return obfuscateImpl(S);
  }

  // For sub classes, new up some new memory for a string. Will terminate
  // The memory with null.
  char *allocateString(unsigned Length) {
    assert(Length > 0 && "no length?");
    char *P = (char *)Alloc.Allocate(Length + 1, 1);
    P[Length] = '\0';
    return P;
  }

  // As StringRef::copy, but will terminate with null
  char *copyString(StringRef S) {
    auto Ret = allocateString(S.size());
    std::copy(S.begin(), S.end(), Ret);
    return Ret;
  }

  virtual ~Obfuscator() {}

public:
  Obfuscator(bool Reversible = false)
      : Alloc(), Reverse(Reversible), ForwardMap(64, Alloc), ReverseMap(Alloc),
        TyForwardMap(64, Alloc) {}

  /// \brief Obfuscate a string
  StringRef obfuscate(StringRef Src) {
    assert(Src.size() && "passed in empty string");

    // See if we've already encountered this value
    if (ForwardMap.count(Src))
      return ForwardMap.lookup(Src);

    auto Res = obfuscateImpl(Src);
    ForwardMap[Src] = Res;
    if (Reverse) {
      // Save a copy of the prior name for reverse mapping
      auto OldName = copyString(Src);
      assert((ReverseMap.lookup(Res) == StringRef() ||
              ReverseMap.lookup(Res) == Src) &&
             "Already entered in, and as a different source. Collision?");
      ReverseMap.insert({Res, OldName});
    }
    return Res;
  }

  /// \brief Obfuscate a type name
  StringRef obfuscateTypeName(StringRef Src) {
    assert(Src.size() && "passed in empty string");

    // See if we've already encountered this value
    if (TyForwardMap.count(Src))
      return TyForwardMap.lookup(Src);

    auto Res = obfuscateTypeNameImpl(Src);
    TyForwardMap[Src] = Res;
    return Res;
  }

  /// \brief Obfuscate the globals and types in a module
  ///
  /// Optionally takes a predicate function set of names to not obfuscate.
  void obfuscateModule(Module &M,
                       std::function<bool(Value &)> preserve);

  /// \brief Whether we're tracking the reverse mapping
  bool providesReverseMapping() const { return Reverse; }

  /// \brief Map an obfuscated symbal back to the original, if it exists
  StringRef getReverseMapping(StringRef Obf) const {
    assert(providesReverseMapping() && "doesn't provide a reverse mapping");
    assert(Obf != ReverseMap.lookup(Obf));
    return ReverseMap.lookup(Obf);
  }

  /// \brief Provide the reverse map, if it exists
  const MapTy &reverseMap() const {
    assert(providesReverseMapping() && "doesn't provide a reverse mapping");
    return ReverseMap;
  }

  /// \brief Write out the reverse map in textural form.
  ///
  /// Derived classes may wish to use a different format
  virtual void writeReverseMap(raw_ostream &OS) {
    for (auto &STE : reverseMap())
      OS << STE.getKey() << ":" << STE.getValue() << "\n";
  }

  // Access the allocator
  BumpPtrAllocator &getAllocator() { return Alloc; }

private:
  Obfuscator(const Obfuscator &) = delete;
  Obfuscator &operator = (const Obfuscator &) = delete;

  virtual void anchor() = 0;
};

/// \brief Simple ROT13 obfuscator
///
/// involutory and stateless
class Rot13Obfuscator : public Obfuscator {
  using Obfuscator::Obfuscator;

  char rot13(char C) {
    if (C >= 'A' && C <= 'Z')
      return ((C - 'A' + 13) % 26) + 'A';
    if (C >= 'a' && C <= 'z')
      return ((C - 'a' + 13) % 26) + 'a';
    return C;
  }

  virtual void anchor() override;

protected:
  StringRef obfuscateImpl(StringRef S) override {
    char *NewStr = allocateString(S.size());
    assert(NewStr[S.size()] == '\0' && "not null terminated");
    for (unsigned i = 0; i < S.size(); ++i)
      NewStr[i] = rot13(S[i]);
    return {NewStr, S.size()};
  }
  StringRef obfuscateTypeNameImpl(StringRef S) override {
    return obfuscateImpl(S);
  }
};

/// \brief Stateful incrementor obfuscator
///
/// Minimal stateful obfuscator, which track the number of obfuscated symbols
/// and appends that number to the end of a prefix. Thus, to get the same
/// resulting obfuscation, this must be fed the inputs in the same order
class IncrementObfuscator : public Obfuscator {
  StringRef Pre;
  StringRef Suf;
  StringRef TyPre;
  unsigned Num = 0;
  unsigned TyNum = 0;

  // Whether we should pad to at least the original length
  bool ShouldPad;

  virtual void anchor() override;

public:
  IncrementObfuscator(bool Reversible = false, bool Pad = false,
                      StringRef Prefix = "__hidden#", StringRef Suffix = "_",
                      StringRef TypePrefix = "__type_hidden#")
      : Obfuscator(Reversible), Pre(copyString(Prefix)),
        Suf(copyString(Suffix)), TyPre(TypePrefix), ShouldPad(Pad) {}

  // Since we're an increment obfuscator, we can just sort the keys and store
  // the values
  void writeReverseMap(raw_ostream &OS) override;

protected:
  StringRef obfuscateImpl(StringRef S) override {
    SmallString<128> NextVal;
    raw_svector_ostream OS(NextVal);

    // If we begin with \01l_OBJC or \01L_OBJC, keep that prefix
    if (S.startswith("\01L") || S.startswith("\01l"))
      OS << S.substr(0, StringRef("\01l").size());

    OS << Pre << Num++ << Suf;
    OS.flush();

    // Pad
    if (ShouldPad && S.size() > NextVal.size())
      NextVal.append(NextPowerOf2(S.size()) - NextVal.size(), '_');

    return copyString(OS.str());
  }

  // Defined separately, so that we don't waste space by having gaps in the
  // reverse mapping
  StringRef obfuscateTypeNameImpl(StringRef S) override {
    SmallString<128> NextVal;
    raw_svector_ostream OS(NextVal);
    OS << TyPre << TyNum++;
    OS.flush();
    // No need to pad
    return copyString(OS.str());
  }

private:
  // Extract the number from a key
  unsigned findIndex(StringRef Key) {
    assert(Key.size() > Pre.size() + Suf.size() && "invalid key");

    size_t Offset = Pre.size();
    if (Key.startswith("\01L") || Key.startswith("\01l"))
      Offset += 2;
    const char *NumStart = Key.data() + Offset;

    size_t Sz = Key.rfind(Suf);
    assert(Sz != StringRef::npos && "out of bounds");
    unsigned long long Ret;
    bool Error = getAsUnsignedInteger({NumStart, Sz - Offset}, 10, Ret);
    (void) Error;
    assert(!Error && "failed to convert to integer");

    return Ret;
  };
};

}
}

#endif // LLVM_TRANSFORMS_UTILS_OBFUSCATION_H
