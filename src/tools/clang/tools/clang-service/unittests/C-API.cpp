#include "clang-service/C-API.h"

#include "gtest/gtest.h"

// FIXME: Actually write the unit tests.

TEST(CAPITest, CXSUID) {
  CXSUID uid = CXSUID_create("foo");
  EXPECT_TRUE(strcmp("foo", CXSUID_get_name(uid)) == 0);
}
