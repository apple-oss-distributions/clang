#include "clang-service/Messaging.h"
#include "clang-service/XPC/Client.h"
#include "clang-service/Support/InProcessClient.h"

#include "llvm/Support/Timer.h"
#include "llvm/Support/raw_ostream.h"

#include "gtest/gtest.h"

using namespace ClangService;
using namespace llvm;

namespace {

struct CommunicationTest : ::testing::Test {
  std::unique_ptr<Client> C;
  std::unique_ptr<Client> InProcC;

#define REGISTER_UID(Name, UIDStr) LazyCSUID Name;
#include "clang-service/ProtocolUIDs.inc"

  void SetUp() {
    C = make_unique<XPC::Client>();
    InProcC = make_unique<InProcessClient>();

#define REGISTER_UID(Name, UIDStr) Name = {UIDStr};
#include "clang-service/ProtocolUIDs.inc"
  }
};

TEST_F(CommunicationTest, Invalid) {
  Value R1 = C->request({});
  EXPECT_TRUE(getResponseErrorKind(R1) == ErrorKind::RequestInvalid);

  Value R2 = C->request(Value::dict({{KeyRequest, false}}));
  EXPECT_TRUE(getResponseErrorKind(R2) == ErrorKind::RequestInvalid);

  Value R3 = C->request(Value::dict({{KeyRequest, " "}}));
  EXPECT_TRUE(getResponseErrorKind(R3) == ErrorKind::RequestInvalid);
}

TEST_F(CommunicationTest, Version) {
  Value R = C->request(Value::dict({{KeyRequest, RequestVersion.c_str()}}));

  auto &V = R.getDict();
  EXPECT_TRUE(V[KeyVersionMinor].isInt64());
  EXPECT_TRUE(V[KeyVersionMajor].isInt64());
}

TEST_F(CommunicationTest, Timer0) {
  TimerGroup TG{"CommunicationTest/Timer0"};
  Timer ColdTimer{"Cold start-up", TG};
  Timer CTimer{"Normal Client", TG};
  Timer InProcCTimer{"In-process Client", TG};

  {
    TimeRegion TR{ColdTimer};
    C->request({});
    InProcC->request({});
  }

  for (unsigned I = 0; I < 10; ++I) {
    TimeRegion TR{CTimer};
    C->request({});
  }

  for (unsigned I = 0; I < 10; ++I) {
    TimeRegion TR{InProcCTimer};
    InProcC->request({});
  }
}

} // end anonymous namespace
