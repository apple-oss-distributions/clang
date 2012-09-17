//===- llvm/unittest/ADT/HashingTest.cpp ----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Hashing.h unit tests.
//
//===----------------------------------------------------------------------===//

#include "gtest/gtest.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/Support/DataTypes.h"
#include <deque>
#include <list>
#include <map>
#include <vector>

namespace llvm {

// Helper for test code to print hash codes.
void PrintTo(const hash_code &code, std::ostream *os) {
  *os << static_cast<size_t>(code);
}

// Fake an object that is recognized as hashable data to test super large
// objects.
struct LargeTestInteger { uint64_t arr[8]; };

namespace hashing {
namespace detail {
template <> struct is_hashable_data<LargeTestInteger> : true_type {};
} // namespace detail
} // namespace hashing

} // namespace llvm

using namespace llvm;

namespace {

TEST(HashingTest, HashValueBasicTest) {
  int x = 42, y = 43, c = 'x';
  void *p = 0;
  uint64_t i = 71;
  const unsigned ci = 71;
  volatile int vi = 71;
  const volatile int cvi = 71;
  uintptr_t addr = reinterpret_cast<uintptr_t>(&y);
  EXPECT_EQ(hash_value(42), hash_value(x));
  EXPECT_NE(hash_value(42), hash_value(y));
  EXPECT_NE(hash_value(42), hash_value(p));
  EXPECT_NE(hash_code::get_null_code(), hash_value(p));
  EXPECT_EQ(hash_value(71), hash_value(i));
  EXPECT_EQ(hash_value(71), hash_value(ci));
  EXPECT_EQ(hash_value(71), hash_value(vi));
  EXPECT_EQ(hash_value(71), hash_value(cvi));
  EXPECT_EQ(hash_value(c), hash_value('x'));
  EXPECT_EQ(hash_value('4'), hash_value('0' + 4));
  EXPECT_EQ(hash_value(addr), hash_value(&y));
}

template <typename T, size_t N> T *begin(T (&arr)[N]) { return arr; }
template <typename T, size_t N> T *end(T (&arr)[N]) { return arr + N; }

// Provide a dummy, hashable type designed for easy verification: its hash is
// the same as its value.
struct HashableDummy { size_t value; };
hash_code hash_value(HashableDummy dummy) { return dummy.value; }

TEST(HashingTest, HashCombineRangeBasicTest) {
  // Leave this uninitialized in the hope that valgrind will catch bad reads.
  int dummy;
  hash_code dummy_hash = hash_combine_range(&dummy, &dummy);
  EXPECT_NE(hash_code::get_null_code(), dummy_hash);
  EXPECT_NE(hash_code::get_invalid_code(), dummy_hash);

  const int arr1[] = { 1, 2, 3 };
  hash_code arr1_hash = hash_combine_range(begin(arr1), end(arr1));
  EXPECT_NE(hash_code::get_null_code(), arr1_hash);
  EXPECT_NE(hash_code::get_invalid_code(), arr1_hash);
  EXPECT_NE(dummy_hash, arr1_hash);
  EXPECT_EQ(arr1_hash, hash_combine_range(begin(arr1), end(arr1)));

  const std::vector<int> vec(begin(arr1), end(arr1));
  EXPECT_EQ(arr1_hash, hash_combine_range(vec.begin(), vec.end()));

  const std::list<int> list(begin(arr1), end(arr1));
  EXPECT_EQ(arr1_hash, hash_combine_range(list.begin(), list.end()));

  const std::deque<int> deque(begin(arr1), end(arr1));
  EXPECT_EQ(arr1_hash, hash_combine_range(deque.begin(), deque.end()));

  const int arr2[] = { 3, 2, 1 };
  hash_code arr2_hash = hash_combine_range(begin(arr2), end(arr2));
  EXPECT_NE(hash_code::get_null_code(), arr2_hash);
  EXPECT_NE(hash_code::get_invalid_code(), arr2_hash);
  EXPECT_NE(dummy_hash, arr2_hash);
  EXPECT_NE(arr1_hash, arr2_hash);

  const int arr3[] = { 1, 1, 2, 3 };
  hash_code arr3_hash = hash_combine_range(begin(arr3), end(arr3));
  EXPECT_NE(hash_code::get_null_code(), arr3_hash);
  EXPECT_NE(hash_code::get_invalid_code(), arr3_hash);
  EXPECT_NE(dummy_hash, arr3_hash);
  EXPECT_NE(arr1_hash, arr3_hash);

  const int arr4[] = { 1, 2, 3, 3 };
  hash_code arr4_hash = hash_combine_range(begin(arr4), end(arr4));
  EXPECT_NE(hash_code::get_null_code(), arr4_hash);
  EXPECT_NE(hash_code::get_invalid_code(), arr4_hash);
  EXPECT_NE(dummy_hash, arr4_hash);
  EXPECT_NE(arr1_hash, arr4_hash);

  const size_t arr5[] = { 1, 2, 3 };
  const HashableDummy d_arr5[] = { {1}, {2}, {3} };
  hash_code arr5_hash = hash_combine_range(begin(arr5), end(arr5));
  hash_code d_arr5_hash = hash_combine_range(begin(d_arr5), end(d_arr5));
  EXPECT_EQ(arr5_hash, d_arr5_hash);
}

TEST(HashingTest, HashCombineRangeLengthDiff) {
  // Test that as only the length varies, we compute different hash codes for
  // sequences.
  std::map<size_t, size_t> code_to_size;
  std::vector<char> all_one_c(256, '\xff');
  for (unsigned Idx = 1, Size = all_one_c.size(); Idx < Size; ++Idx) {
    hash_code code = hash_combine_range(&all_one_c[0], &all_one_c[0] + Idx);
    std::map<size_t, size_t>::iterator
      I = code_to_size.insert(std::make_pair(code, Idx)).first;
    EXPECT_EQ(Idx, I->second);
  }
  code_to_size.clear();
  std::vector<char> all_zero_c(256, '\0');
  for (unsigned Idx = 1, Size = all_zero_c.size(); Idx < Size; ++Idx) {
    hash_code code = hash_combine_range(&all_zero_c[0], &all_zero_c[0] + Idx);
    std::map<size_t, size_t>::iterator
      I = code_to_size.insert(std::make_pair(code, Idx)).first;
    EXPECT_EQ(Idx, I->second);
  }
  code_to_size.clear();
  std::vector<unsigned> all_one_int(512, -1);
  for (unsigned Idx = 1, Size = all_one_int.size(); Idx < Size; ++Idx) {
    hash_code code = hash_combine_range(&all_one_int[0], &all_one_int[0] + Idx);
    std::map<size_t, size_t>::iterator
      I = code_to_size.insert(std::make_pair(code, Idx)).first;
    EXPECT_EQ(Idx, I->second);
  }
  code_to_size.clear();
  std::vector<unsigned> all_zero_int(512, 0);
  for (unsigned Idx = 1, Size = all_zero_int.size(); Idx < Size; ++Idx) {
    hash_code code = hash_combine_range(&all_zero_int[0], &all_zero_int[0] + Idx);
    std::map<size_t, size_t>::iterator
      I = code_to_size.insert(std::make_pair(code, Idx)).first;
    EXPECT_EQ(Idx, I->second);
  }
}

TEST(HashingTest, HashCombineRangeGoldenTest) {
  struct { const char *s; uint64_t hash; } golden_data[] = {
    { "a",                                0xaeb6f9d5517c61f8ULL },
    { "ab",                               0x7ab1edb96be496b4ULL },
    { "abc",                              0xe38e60bf19c71a3fULL },
    { "abcde",                            0xd24461a66de97f6eULL },
    { "abcdefgh",                         0x4ef872ec411dec9dULL },
    { "abcdefghijklm",                    0xe8a865539f4eadfeULL },
    { "abcdefghijklmnopqrstu",            0x261cdf85faaf4e79ULL },
    { "abcdefghijklmnopqrstuvwxyzabcdef", 0x43ba70e4198e3b2aULL },
    { "abcdefghijklmnopqrstuvwxyzabcdef"
      "abcdefghijklmnopqrstuvwxyzghijkl"
      "abcdefghijklmnopqrstuvwxyzmnopqr"
      "abcdefghijklmnopqrstuvwxyzstuvwx"
      "abcdefghijklmnopqrstuvwxyzyzabcd", 0xdcd57fb2afdf72beULL },
    { "a",                                0xaeb6f9d5517c61f8ULL },
    { "aa",                               0xf2b3b69a9736a1ebULL },
    { "aaa",                              0xf752eb6f07b1cafeULL },
    { "aaaaa",                            0x812bd21e1236954cULL },
    { "aaaaaaaa",                         0xff07a2cff08ac587ULL },
    { "aaaaaaaaaaaaa",                    0x84ac949d54d704ecULL },
    { "aaaaaaaaaaaaaaaaaaaaa",            0xcb2c8fb6be8f5648ULL },
    { "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0xcc40ab7f164091b6ULL },
    { "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0xc58e174c1e78ffe9ULL },
    { "z",                                0x1ba160d7e8f8785cULL },
    { "zz",                               0x2c5c03172f1285d7ULL },
    { "zzz",                              0x9d2c4f4b507a2ac3ULL },
    { "zzzzz",                            0x0f03b9031735693aULL },
    { "zzzzzzzz",                         0xe674147c8582c08eULL },
    { "zzzzzzzzzzzzz",                    0x3162d9fa6938db83ULL },
    { "zzzzzzzzzzzzzzzzzzzzz",            0x37b9a549e013620cULL },
    { "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz", 0x8921470aff885016ULL },
    { "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz"
      "zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz", 0xf60fdcd9beb08441ULL },
    { "a",                                0xaeb6f9d5517c61f8ULL },
    { "ab",                               0x7ab1edb96be496b4ULL },
    { "aba",                              0x3edb049950884d0aULL },
    { "ababa",                            0x8f2de9e73a97714bULL },
    { "abababab",                         0xee14a29ddf0ce54cULL },
    { "ababababababa",                    0x38b3ddaada2d52b4ULL },
    { "ababababababababababa",            0xd3665364219f2b85ULL },
    { "abababababababababababababababab"
      "abababababababababababababababab"
      "abababababababababababababababab"
      "abababababababababababababababab"
      "abababababababababababababababab", 0x840192d129f7a22bULL }
  };
  for (unsigned i = 0; i < sizeof(golden_data)/sizeof(*golden_data); ++i) {
    StringRef str = golden_data[i].s;
    hash_code hash = hash_combine_range(str.begin(), str.end());
#if 0 // Enable this to generate paste-able text for the above structure.
    std::string member_str = "\"" + str.str() + "\",";
    fprintf(stderr, " { %-35s 0x%016lxULL },\n",
            member_str.c_str(), (size_t)hash);
#endif
    EXPECT_EQ(static_cast<size_t>(golden_data[i].hash),
              static_cast<size_t>(hash));
  }
}

TEST(HashingTest, HashCombineBasicTest) {
  // Hashing a sequence of homogenous types matches range hashing.
  const int i1 = 42, i2 = 43, i3 = 123, i4 = 999, i5 = 0, i6 = 79;
  const int arr1[] = { i1, i2, i3, i4, i5, i6 };
  EXPECT_EQ(hash_combine_range(arr1, arr1 + 1), hash_combine(i1));
  EXPECT_EQ(hash_combine_range(arr1, arr1 + 2), hash_combine(i1, i2));
  EXPECT_EQ(hash_combine_range(arr1, arr1 + 3), hash_combine(i1, i2, i3));
  EXPECT_EQ(hash_combine_range(arr1, arr1 + 4), hash_combine(i1, i2, i3, i4));
  EXPECT_EQ(hash_combine_range(arr1, arr1 + 5),
            hash_combine(i1, i2, i3, i4, i5));
  EXPECT_EQ(hash_combine_range(arr1, arr1 + 6),
            hash_combine(i1, i2, i3, i4, i5, i6));

  // Hashing a sequence of heterogenous types which *happen* to all produce the
  // same data for hashing produces the same as a range-based hash of the
  // fundamental values.
  const size_t s1 = 1024, s2 = 8888, s3 = 9000000;
  const HashableDummy d1 = { 1024 }, d2 = { 8888 }, d3 = { 9000000 };
  const size_t arr2[] = { s1, s2, s3 };
  EXPECT_EQ(hash_combine_range(begin(arr2), end(arr2)),
            hash_combine(s1, s2, s3));
  EXPECT_EQ(hash_combine(s1, s2, s3), hash_combine(s1, s2, d3));
  EXPECT_EQ(hash_combine(s1, s2, s3), hash_combine(s1, d2, s3));
  EXPECT_EQ(hash_combine(s1, s2, s3), hash_combine(d1, s2, s3));
  EXPECT_EQ(hash_combine(s1, s2, s3), hash_combine(d1, d2, s3));
  EXPECT_EQ(hash_combine(s1, s2, s3), hash_combine(d1, d2, d3));

  // Permuting values causes hashes to change.
  EXPECT_NE(hash_combine(i1, i1, i1), hash_combine(i1, i1, i2));
  EXPECT_NE(hash_combine(i1, i1, i1), hash_combine(i1, i2, i1));
  EXPECT_NE(hash_combine(i1, i1, i1), hash_combine(i2, i1, i1));
  EXPECT_NE(hash_combine(i1, i1, i1), hash_combine(i2, i2, i1));
  EXPECT_NE(hash_combine(i1, i1, i1), hash_combine(i2, i2, i2));
  EXPECT_NE(hash_combine(i2, i1, i1), hash_combine(i1, i1, i2));
  EXPECT_NE(hash_combine(i1, i1, i2), hash_combine(i1, i2, i1));
  EXPECT_NE(hash_combine(i1, i2, i1), hash_combine(i2, i1, i1));

  // Changing type w/o changing value causes hashes to change.
  EXPECT_NE(hash_combine(i1, i2, i3), hash_combine((char)i1, i2, i3));
  EXPECT_NE(hash_combine(i1, i2, i3), hash_combine(i1, (char)i2, i3));
  EXPECT_NE(hash_combine(i1, i2, i3), hash_combine(i1, i2, (char)i3));

  // This is array of uint64, but it should have the exact same byte pattern as
  // an array of LargeTestIntegers.
  const uint64_t bigarr[] = {
    0xaaaaaaaaababababULL, 0xacacacacbcbcbcbcULL, 0xccddeeffeeddccbbULL,
    0xdeadbeafdeadbeefULL, 0xfefefefededededeULL, 0xafafafafededededULL,
    0xffffeeeeddddccccULL, 0xaaaacbcbffffababULL,
    0xaaaaaaaaababababULL, 0xacacacacbcbcbcbcULL, 0xccddeeffeeddccbbULL,
    0xdeadbeafdeadbeefULL, 0xfefefefededededeULL, 0xafafafafededededULL,
    0xffffeeeeddddccccULL, 0xaaaacbcbffffababULL,
    0xaaaaaaaaababababULL, 0xacacacacbcbcbcbcULL, 0xccddeeffeeddccbbULL,
    0xdeadbeafdeadbeefULL, 0xfefefefededededeULL, 0xafafafafededededULL,
    0xffffeeeeddddccccULL, 0xaaaacbcbffffababULL
  };
  // Hash a preposterously large integer, both aligned with the buffer and
  // misaligned.
  const LargeTestInteger li = { {
    0xaaaaaaaaababababULL, 0xacacacacbcbcbcbcULL, 0xccddeeffeeddccbbULL,
    0xdeadbeafdeadbeefULL, 0xfefefefededededeULL, 0xafafafafededededULL,
    0xffffeeeeddddccccULL, 0xaaaacbcbffffababULL
  } };
  // Rotate the storage from 'li'.
  const LargeTestInteger l2 = { {
    0xacacacacbcbcbcbcULL, 0xccddeeffeeddccbbULL, 0xdeadbeafdeadbeefULL,
    0xfefefefededededeULL, 0xafafafafededededULL, 0xffffeeeeddddccccULL,
    0xaaaacbcbffffababULL, 0xaaaaaaaaababababULL
  } };
  const LargeTestInteger l3 = { {
    0xccddeeffeeddccbbULL, 0xdeadbeafdeadbeefULL, 0xfefefefededededeULL,
    0xafafafafededededULL, 0xffffeeeeddddccccULL, 0xaaaacbcbffffababULL,
    0xaaaaaaaaababababULL, 0xacacacacbcbcbcbcULL
  } };
  EXPECT_EQ(hash_combine_range(begin(bigarr), end(bigarr)),
            hash_combine(li, li, li));
  EXPECT_EQ(hash_combine_range(bigarr, bigarr + 9),
            hash_combine(bigarr[0], l2));
  EXPECT_EQ(hash_combine_range(bigarr, bigarr + 10),
            hash_combine(bigarr[0], bigarr[1], l3));
  EXPECT_EQ(hash_combine_range(bigarr, bigarr + 17),
            hash_combine(li, bigarr[0], l2));
  EXPECT_EQ(hash_combine_range(bigarr, bigarr + 18),
            hash_combine(li, bigarr[0], bigarr[1], l3));
  EXPECT_EQ(hash_combine_range(bigarr, bigarr + 18),
            hash_combine(bigarr[0], l2, bigarr[9], l3));
  EXPECT_EQ(hash_combine_range(bigarr, bigarr + 20),
            hash_combine(bigarr[0], l2, bigarr[9], l3, bigarr[18], bigarr[19]));
}

}