#include "clang-service/XPC/Service.h"

int main() {
  auto &Server = XPC::Service::get();
  Server.serve();
  return EXIT_FAILURE;
}
