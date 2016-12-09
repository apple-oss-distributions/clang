#include "clang-service/Support/Value.h"
#include "clang-service/XPC/XPC.h"

#include "gtest/gtest.h"

#include "llvm/Support/raw_ostream.h"
#include <limits>

using namespace ClangService;
using llvm::errs;
using llvm::StringRef;

namespace {

bool checkIfEqual(Value &Before, Value &After) {
  if (Before == After)
    return true;

  errs() << "Mismatch between value and clone!\n";
  errs() << "  Before (structure): "; Before.structure(errs());
  errs() << "  After  (structure): "; After.structure(errs());
  errs() << "  Before (dump)     : "; Before.dump(errs());
  errs() << "  After  (dump)     : "; After.dump(errs());
  assert(!"Mismatched values");
  return false;
}

bool roundTripAndValidate(Value V) {
  auto Before = V.clone();
  assert(checkIfEqual(V, Before) && "Value clone is incorrect");
  auto After = XPC::deserialize(XPC::serialize(std::move(V)));
  return checkIfEqual(Before, After);
}

TEST(RoundTripTest, NullKind) {
  EXPECT_TRUE(roundTripAndValidate({}));
}

TEST(RoundTripTest, BoolKind) {
  EXPECT_TRUE(roundTripAndValidate(true));
  EXPECT_TRUE(roundTripAndValidate(false));
}

TEST(RoundTripTest, Int64Kind) {
  EXPECT_TRUE(roundTripAndValidate(0LL));
  EXPECT_TRUE(roundTripAndValidate(std::numeric_limits<int64_t>::min()));
  EXPECT_TRUE(roundTripAndValidate(std::numeric_limits<int64_t>::max()));
}

TEST(RoundTripTest, UIDKind) {
  CSUID X{"foo"};
  LazyCSUID Y{"bix"};
  EXPECT_TRUE(roundTripAndValidate({X}));
  EXPECT_TRUE(roundTripAndValidate({Y}));
}

TEST(RoundTripTest, StringKind) {
  EXPECT_TRUE(roundTripAndValidate(""));
  EXPECT_TRUE(roundTripAndValidate("bar"));
  EXPECT_TRUE(roundTripAndValidate("baz quux"));
}

TEST(RoundTripTest, OwnedStringKind) {
  auto OS1 = InlineOwnedString::create("boop");
  EXPECT_TRUE(roundTripAndValidate(Value::string(std::move(OS1))));

  auto IOS = InlineOwnedString::create(5);
  {
    raw_inlinestring_ostream Stream{*IOS.get()};
    Stream << '\0';
    Stream << StringRef("xuz");
    Stream << '?';
  }
  EXPECT_TRUE(IOS->get().equals({"\0xuz?", 5}));
  std::unique_ptr<OwnedString> OS2(IOS.release());
  EXPECT_TRUE(roundTripAndValidate(Value::data(std::move(OS2))));
}

TEST(RoundTripTest, DataKind) {
  EXPECT_TRUE(roundTripAndValidate(Value::data("data", 4)));
  EXPECT_TRUE(roundTripAndValidate(Value::data("data", 5)));
  EXPECT_TRUE(roundTripAndValidate(Value::data(" \x01\x02", 3)));
  EXPECT_TRUE(roundTripAndValidate(Value::data(" \x01\x02", 4)));
  EXPECT_TRUE(roundTripAndValidate(Value::data(" \x00\x01\x02", 4)));
  EXPECT_TRUE(roundTripAndValidate(Value::data(" \x00\x01\x02", 5)));
  EXPECT_TRUE(roundTripAndValidate(Value::data("\x00\r\x00\n\x00\t", 6)));
  EXPECT_TRUE(roundTripAndValidate(Value::data("\x00\r\x00\n\x00\t", 7)));
}

TEST(RoundTripTest, OwnedDataKind) {
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({"data", 4}))));
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({"data", 5}))));
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({" \x01\x02", 3}))));
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({" \x01\x02", 4}))));
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({" \x00\x01\x02", 4}))));
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({" \x00\x01\x02", 5}))));
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({"\x00\r\x00\n\x00\t", 6}))));
  EXPECT_TRUE(roundTripAndValidate(
      Value::data(InlineOwnedString::create({"\x00\r\x00\n\x00\t", 7}))));
}

TEST(RoundTripTest, ArrayKind) {
  CSUID K{"k1"};
  auto A1 = Value::array({});
  auto A2 = Value::array({A1.clone()});
  auto A3 = Value::array({Value::dict({{K, A2.clone()}})});
  auto A4 = Value::array({Value(), false, 0LL, K, "boop beep"});
  auto A5 = Value::array({A3.clone(), A4.clone()});
  EXPECT_TRUE(roundTripAndValidate(std::move(A1)));
  EXPECT_TRUE(roundTripAndValidate(std::move(A2)));
  EXPECT_TRUE(roundTripAndValidate(std::move(A3)));
  EXPECT_TRUE(roundTripAndValidate(std::move(A4)));
  EXPECT_TRUE(roundTripAndValidate(std::move(A5)));
}

TEST(RoundTripTest, DictKind) {
  CSUID K{"k2"};
  LazyCSUID Q{"k3"};
  auto D1 = Value::dict({});
  auto D2 = Value::dict({{K, D1.clone()}});
  auto D3 = Value::dict({{K, Value::array({})}, {Q, D2.clone()}});
  auto D4 = Value::dict({{CSUID("k4"), Value()},
                         {CSUID("k5"), false},
                         {CSUID("k6"), 0LL},
                         {CSUID("k7"), K},
                         {CSUID("k8"), "boop beep"}});
  auto D5 = Value::dict({{K, D3.clone()}, {Q, D4.clone()}});
  EXPECT_TRUE(roundTripAndValidate(std::move(D1)));
  EXPECT_TRUE(roundTripAndValidate(std::move(D2)));
  EXPECT_TRUE(roundTripAndValidate(std::move(D3)));
  EXPECT_TRUE(roundTripAndValidate(std::move(D4)));
  EXPECT_TRUE(roundTripAndValidate(std::move(D5)));
}

TEST(RoundTripTest, SaneExtraction) {
  auto A1 = Value::array({Value::array({true})});
  auto A2 = std::move(A1.getArray()[0]);
  EXPECT_TRUE(roundTripAndValidate(std::move(A1)));
  EXPECT_TRUE(roundTripAndValidate(std::move(A2)));
}

} // end anonymous namespace
