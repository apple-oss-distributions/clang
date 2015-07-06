//===- llvm/ADT/DenseMapInfo.h - Type traits for DenseMap -------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines DenseMapInfo traits for DenseMap.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_DENSEMAPINFO_H
#define LLVM_ADT_DENSEMAPINFO_H

#include "llvm/Support/PointerLikeTypeTraits.h"
#include "llvm/Support/type_traits.h"
#include <tuple>

namespace llvm {

template<typename T>
struct DenseMapInfo {
  //static inline T getEmptyKey();
  //static inline T getTombstoneKey();
  //static unsigned getHashValue(const T &Val);
  //static bool isEqual(const T &LHS, const T &RHS);
};

// Provide DenseMapInfo for all pointers.
template<typename T>
struct DenseMapInfo<T*> {
  static inline T* getEmptyKey() {
    uintptr_t Val = static_cast<uintptr_t>(-1);
    Val <<= PointerLikeTypeTraits<T*>::NumLowBitsAvailable;
    return reinterpret_cast<T*>(Val);
  }
  static inline T* getTombstoneKey() {
    uintptr_t Val = static_cast<uintptr_t>(-2);
    Val <<= PointerLikeTypeTraits<T*>::NumLowBitsAvailable;
    return reinterpret_cast<T*>(Val);
  }
  static unsigned getHashValue(const T *PtrVal) {
    return (unsigned((uintptr_t)PtrVal) >> 4) ^
           (unsigned((uintptr_t)PtrVal) >> 9);
  }
  static bool isEqual(const T *LHS, const T *RHS) { return LHS == RHS; }
};

// Provide DenseMapInfo for chars.
template<> struct DenseMapInfo<char> {
  static inline char getEmptyKey() { return ~0; }
  static inline char getTombstoneKey() { return ~0 - 1; }
  static unsigned getHashValue(const char& Val) { return Val * 37U; }
  static bool isEqual(const char &LHS, const char &RHS) {
    return LHS == RHS;
  }
};
  
// Provide DenseMapInfo for unsigned ints.
template<> struct DenseMapInfo<unsigned> {
  static inline unsigned getEmptyKey() { return ~0U; }
  static inline unsigned getTombstoneKey() { return ~0U - 1; }
  static unsigned getHashValue(const unsigned& Val) { return Val * 37U; }
  static bool isEqual(const unsigned& LHS, const unsigned& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for unsigned longs.
template<> struct DenseMapInfo<unsigned long> {
  static inline unsigned long getEmptyKey() { return ~0UL; }
  static inline unsigned long getTombstoneKey() { return ~0UL - 1L; }
  static unsigned getHashValue(const unsigned long& Val) {
    return (unsigned)(Val * 37UL);
  }
  static bool isEqual(const unsigned long& LHS, const unsigned long& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for unsigned long longs.
template<> struct DenseMapInfo<unsigned long long> {
  static inline unsigned long long getEmptyKey() { return ~0ULL; }
  static inline unsigned long long getTombstoneKey() { return ~0ULL - 1ULL; }
  static unsigned getHashValue(const unsigned long long& Val) {
    return (unsigned)(Val * 37ULL);
  }
  static bool isEqual(const unsigned long long& LHS,
                      const unsigned long long& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for ints.
template<> struct DenseMapInfo<int> {
  static inline int getEmptyKey() { return 0x7fffffff; }
  static inline int getTombstoneKey() { return -0x7fffffff - 1; }
  static unsigned getHashValue(const int& Val) { return (unsigned)(Val * 37U); }
  static bool isEqual(const int& LHS, const int& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for longs.
template<> struct DenseMapInfo<long> {
  static inline long getEmptyKey() {
    return (1UL << (sizeof(long) * 8 - 1)) - 1UL;
  }
  static inline long getTombstoneKey() { return getEmptyKey() - 1L; }
  static unsigned getHashValue(const long& Val) {
    return (unsigned)(Val * 37UL);
  }
  static bool isEqual(const long& LHS, const long& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for long longs.
template<> struct DenseMapInfo<long long> {
  static inline long long getEmptyKey() { return 0x7fffffffffffffffLL; }
  static inline long long getTombstoneKey() { return -0x7fffffffffffffffLL-1; }
  static unsigned getHashValue(const long long& Val) {
    return (unsigned)(Val * 37ULL);
  }
  static bool isEqual(const long long& LHS,
                      const long long& RHS) {
    return LHS == RHS;
  }
};

/// Simplistic combination of 32-bit hash values into 32-bit hash values.
static inline unsigned combineHashValue(unsigned a, unsigned b) {
  uint64_t key = (uint64_t)a << 32 | (uint64_t)b;
  key += ~(key << 32);
  key ^= (key >> 22);
  key += ~(key << 13);
  key ^= (key >> 8);
  key += (key << 3);
  key ^= (key >> 15);
  key += ~(key << 27);
  key ^= (key >> 31);
  return (unsigned)key;
}

// Provide DenseMapInfo for all pairs whose members have info.
template<typename T, typename U>
struct DenseMapInfo<std::pair<T, U> > {
  typedef std::pair<T, U> Pair;
  typedef DenseMapInfo<T> FirstInfo;
  typedef DenseMapInfo<U> SecondInfo;

  static inline Pair getEmptyKey() {
    return std::make_pair(FirstInfo::getEmptyKey(),
                          SecondInfo::getEmptyKey());
  }
  static inline Pair getTombstoneKey() {
    return std::make_pair(FirstInfo::getTombstoneKey(),
                          SecondInfo::getTombstoneKey());
  }
  static unsigned getHashValue(const Pair& PairVal) {
    return combineHashValue(FirstInfo::getHashValue(PairVal.first),
                            SecondInfo::getHashValue(PairVal.second));
  }
  static bool isEqual(const Pair &LHS, const Pair &RHS) {
    return FirstInfo::isEqual(LHS.first, RHS.first) &&
           SecondInfo::isEqual(LHS.second, RHS.second);
  }
};

#if LLVM_HAS_VARIADIC_TEMPLATES
template<typename ...Ts>
struct DenseMapInfo<std::tuple<Ts...> > {
  typedef std::tuple<Ts...> Tuple;

  /// Helper class
  template<unsigned N> struct UnsignedC { };

  static inline Tuple getEmptyKey() {
    return Tuple(DenseMapInfo<Ts>::getEmptyKey()...);
  }

  static inline Tuple getTombstoneKey() {
    return Tuple(DenseMapInfo<Ts>::getTombstoneKey()...);
  }

  template<unsigned I>
  static unsigned getHashValueImpl(const Tuple& values, std::false_type) {
    typedef typename std::tuple_element<I, Tuple>::type EltType;
    std::integral_constant<bool, I+1 == sizeof...(Ts)> atEnd;
    return combineHashValue(
             DenseMapInfo<EltType>::getHashValue(std::get<I>(values)),
             getHashValueImpl<I+1>(values, atEnd));
  }

  template<unsigned I>
  static unsigned getHashValueImpl(const Tuple& values, std::true_type) {
    return 0;
  }

  static unsigned getHashValue(const std::tuple<Ts...>& values) {
    std::integral_constant<bool, 0 == sizeof...(Ts)> atEnd;
    return getHashValueImpl<0>(values, atEnd);
  }

  template<unsigned I>
  static bool isEqualImpl(const Tuple &lhs, const Tuple &rhs, std::false_type) {
    typedef typename std::tuple_element<I, Tuple>::type EltType;
    std::integral_constant<bool, I+1 == sizeof...(Ts)> atEnd;
    return DenseMapInfo<EltType>::isEqual(std::get<I>(lhs), std::get<I>(rhs))
           && isEqualImpl<I+1>(lhs, rhs, atEnd);
  }

  template<unsigned I>
  static bool isEqualImpl(const Tuple &lhs, const Tuple &rhs, std::true_type) {
    return true;
  }

  static bool isEqual(const Tuple &lhs, const Tuple &rhs) {
    std::integral_constant<bool, 0 == sizeof...(Ts)> atEnd;
    return isEqualImpl<0>(lhs, rhs, atEnd);
  }
};
#else
template<typename T1>
struct DenseMapInfo<std::tuple<T1> > {
  typedef std::tuple<T1> Tuple;

  /// Helper class
  static inline Tuple getEmptyKey() {
    return Tuple(DenseMapInfo<T1>::getEmptyKey());
  }

  static inline Tuple getTombstoneKey() {
    return Tuple(DenseMapInfo<T1>::getTombstoneKey());
  }

  static unsigned getHashValue(const Tuple& values) {
    return DenseMapInfo<T1>::getHashValue(std::get<0>(values));
  }

  static bool isEqual(const Tuple &lhs, const Tuple &rhs) {
    return DenseMapInfo<T1>::isEqual(std::get<0>(lhs), std::get<0>(rhs));
  }
};

template<typename T1, typename T2>
struct DenseMapInfo<std::tuple<T1, T2> > {
  typedef std::tuple<T1, T2> Tuple;

  /// Helper class
  static inline Tuple getEmptyKey() {
    return Tuple(DenseMapInfo<T1>::getEmptyKey(),
                 DenseMapInfo<T2>::getEmptyKey());
  }

  static inline Tuple getTombstoneKey() {
    return Tuple(DenseMapInfo<T1>::getTombstoneKey(),
                 DenseMapInfo<T2>::getTombstoneKey());
  }

  static unsigned getHashValue(const Tuple& values) {
    return combineHashValue(
             DenseMapInfo<T1>::getHashValue(std::get<0>(values)),
             DenseMapInfo<T2>::getHashValue(std::get<1>(values)));
  }

  static bool isEqual(const Tuple &lhs, const Tuple &rhs) {
    return DenseMapInfo<T1>::isEqual(std::get<0>(lhs), std::get<0>(rhs)) &&
           DenseMapInfo<T2>::isEqual(std::get<1>(lhs), std::get<1>(rhs));
  }
};

template<typename T1, typename T2, typename T3>
struct DenseMapInfo<std::tuple<T1, T2, T3> > {
  typedef std::tuple<T1, T2, T3> Tuple;

  /// Helper class
  static inline Tuple getEmptyKey() {
    return Tuple(DenseMapInfo<T1>::getEmptyKey(),
                 DenseMapInfo<T2>::getEmptyKey(),
                 DenseMapInfo<T3>::getEmptyKey());
  }

  static inline Tuple getTombstoneKey() {
    return Tuple(DenseMapInfo<T1>::getTombstoneKey(),
                 DenseMapInfo<T2>::getTombstoneKey(),
                 DenseMapInfo<T3>::getTombstoneKey());
  }

  static unsigned getHashValue(const Tuple& values) {
    unsigned result = DenseMapInfo<T1>::getHashValue(std::get<0>(values));
    result = combineHashValue(
               result,
               DenseMapInfo<T2>::getHashValue(std::get<1>(values)));
    result = combineHashValue(
               result,
               DenseMapInfo<T3>::getHashValue(std::get<2>(values)));
    return result;
  }

  static bool isEqual(const Tuple &lhs, const Tuple &rhs) {
    return DenseMapInfo<T1>::isEqual(std::get<0>(lhs), std::get<0>(rhs)) &&
           DenseMapInfo<T2>::isEqual(std::get<1>(lhs), std::get<1>(rhs)) &&
           DenseMapInfo<T3>::isEqual(std::get<2>(lhs), std::get<2>(rhs));
  }
};

template<typename T1, typename T2, typename T3, typename T4>
struct DenseMapInfo<std::tuple<T1, T2, T3, T4> > {
  typedef std::tuple<T1, T2, T3, T4> Tuple;

  /// Helper class
  static inline Tuple getEmptyKey() {
    return Tuple(DenseMapInfo<T1>::getEmptyKey(),
                 DenseMapInfo<T2>::getEmptyKey(),
                 DenseMapInfo<T3>::getEmptyKey(),
                 DenseMapInfo<T4>::getEmptyKey());
  }

  static inline Tuple getTombstoneKey() {
    return Tuple(DenseMapInfo<T1>::getTombstoneKey(),
                 DenseMapInfo<T2>::getTombstoneKey(),
                 DenseMapInfo<T3>::getTombstoneKey(),
                 DenseMapInfo<T4>::getTombstoneKey());
  }

  static unsigned getHashValue(const Tuple& values) {
    unsigned result = DenseMapInfo<T1>::getHashValue(std::get<0>(values));
    result = combineHashValue(
               result,
               DenseMapInfo<T2>::getHashValue(std::get<1>(values)));
    result = combineHashValue(
               result,
               DenseMapInfo<T3>::getHashValue(std::get<2>(values)));
    result = combineHashValue(
               result,
               DenseMapInfo<T4>::getHashValue(std::get<3>(values)));
    return result;
  }

  static bool isEqual(const Tuple &lhs, const Tuple &rhs) {
    return DenseMapInfo<T1>::isEqual(std::get<0>(lhs), std::get<0>(rhs)) &&
           DenseMapInfo<T2>::isEqual(std::get<1>(lhs), std::get<1>(rhs)) &&
           DenseMapInfo<T3>::isEqual(std::get<2>(lhs), std::get<2>(rhs)) &&
           DenseMapInfo<T4>::isEqual(std::get<3>(lhs), std::get<3>(rhs));
  }
};
#endif

} // end namespace llvm

#endif
