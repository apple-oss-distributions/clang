#include "clang-service/Support/UID.h"

#include "llvm/ADT/StringMap.h"
#include "llvm/Support/ManagedStatic.h"

#include <mutex>

namespace ClangService {

using llvm::StringRef;

class UIDRegistry {
  llvm::StringMap<void *> Map;
  std::mutex Lock;

public:
  using UIDMapping = llvm::StringMapEntry<void *>;

  /// Get or create a UIDMapping for \p S.
  ///
  /// N.b that this *always* copies \p S into the registry.
  UIDMapping *mapUID(StringRef S) {
    std::unique_lock<std::mutex> Guard(Lock);
    auto It = Map.find(S);
    if (It != Map.end())
      return &*It;
    auto Pair = Map.insert(std::make_pair(S, nullptr));
    return &*Pair.first;
  }
};

llvm::ManagedStatic<UIDRegistry> GlobalUIDRegistry;

CSUID::CSUID(StringRef S) {
  assert(isValidIdentifier(S));
  UIDRegistry &UR = *GlobalUIDRegistry;
  auto *UM = UR.mapUID(S);
  UID = reinterpret_cast<void *>(UM);
}

StringRef CSUID::getName() const {
  auto *UM = reinterpret_cast<UIDRegistry::UIDMapping *>(UID);
  return UM->getKey();
}

void *CSUID::getTag() const {
  auto *UM = reinterpret_cast<UIDRegistry::UIDMapping *>(UID);
  return UM->getValue();
}

void CSUID::setTag(void *Tag) const {
  auto *UM = reinterpret_cast<UIDRegistry::UIDMapping *>(UID);
  return UM->setValue(Tag);
}

uint64_t CSUID::getAsU64() const {
  auto IP = reinterpret_cast<intptr_t>(UID);
  return reinterpret_cast<uint64_t>(IP & ~(uint64_t()));
}

CSUID CSUID::getFromU64(uint64_t ID) {
  auto *UM = reinterpret_cast<UIDRegistry::UIDMapping *>(ID);
  assert(GlobalUIDRegistry->mapUID(UM->getKey()) == UM &&
         "Not a valid CSUID (no matching UIDMapping found)");
  return {reinterpret_cast<void *>(UM)};
}

bool CSUID::isValidIdentifier(StringRef S) {
  return S.find(' ') == StringRef::npos;
}

} // end namespace ClangService
