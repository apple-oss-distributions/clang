#include "XarAnalyzer.h"

extern "C" {
#include <xar/xar.h>
}

#include <unistd.h>
#include <libxml/parser.h>
#include <libxml/xpath.h>
#include <libxml/xpathInternals.h>
#include <libxml/tree.h>

#include "llvm/APIAnalysis.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

using namespace llvm;
using namespace object;

int ExtractXar(xar_t x, std::string &tmpDir) {

  xar_iter_t it = xar_iter_new();
  if (!it) {
    errs() << "Could not construct xar iter\n";
    return 1;
  }
  for (xar_file_t f = xar_file_first(x, it); f; f = xar_file_next(it)) {
    xar_extract(x, f);
  }

  xar_iter_free(it);
  return 0;
}

int ProcessXarHeader(xar_t x, APIAnalysisIntermediateResult &result) {
  xar_serialize(x, "tocfile.xml");

  xmlInitParser();

  xmlDocPtr doc;
  xmlXPathContextPtr xpathCtx;
  xmlXPathObjectPtr xpathObj;

  /* Load XML document */
  doc = xmlParseFile("tocfile.xml");
  if (!doc) {
    errs() << "Error: unable to parse tocfile";
    return 1;
  }

  /* Create xpath evaluation context */
  xpathCtx = xmlXPathNewContext(doc);
  if (!xpathCtx) {
    errs() << "Error: unable to create new XPath context\n";
    xmlFreeDoc(doc);
    return 1;
  }

  /* Evaluate xpath expression */
  const xmlChar *xpathExpr = (const xmlChar *)"/xar/subdoc/dylibs/lib";
  xpathObj = xmlXPathEvalExpression(xpathExpr, xpathCtx);
  if (!xpathObj) {
    errs() << "Error: unable to evaluate xpath expression.";
    xmlXPathFreeContext(xpathCtx);
    xmlFreeDoc(doc);
    return 1;
  }

  auto nodes = xpathObj->nodesetval;
  int size = (nodes) ? nodes->nodeNr : 0;
  for (int i = 0; i < size; ++i) {
    if (nodes->nodeTab[i]->type == XML_ELEMENT_NODE) {
      StringRef libName((char *)nodes->nodeTab[i]->children->content);
      if (libName.startswith("{SDKPATH}//"))
        libName = libName.drop_front(10);
      result.linkedLibraries.insert(libName.str());
      result.orderedLibraries.push_back(libName.str());
    }
  }

  /* Cleanup */
  xmlXPathFreeObject(xpathObj);
  xmlXPathFreeContext(xpathCtx);
  xmlFreeDoc(doc);

  xmlCleanupParser();

  sys::fs::remove("tocfile.xml");
  return 0;
}

int AnalyzeXar(StringRef &filePath, APIAnalysisIntermediateResult &result,
               const APIAnalysisOptions &options) {

  char buf[4096];
  if (!getcwd(buf, 4096)) {
    errs() << "Could not identify cwd\n";
    return 1;
  }

  std::string tmpDir = filePath.str() + ".dir";
  if (std::error_code EC = sys::fs::create_directory(tmpDir)) {
    errs() << "Could not create a directory to expand into." << EC.message()
           << "\n";
    return 1;
  }

  xar_t x;
  x = xar_open(filePath.data(), READ);
  if (x == NULL) {
    errs() << "Error opening xarchive: " << filePath << "\n";
    return 1;
  }

  if (chdir(tmpDir.c_str())) {
    errs() << "Could not change dir.\n";
    return 1;
  }
  auto success = ExtractXar(x, tmpDir);
  if (0 != success) {
    chdir(buf);
    errs() << "Extracting xar failed.\n";
    return success;
  }

  success = ProcessXarHeader(x, result);
  chdir(buf);
  if (0 != success) {
    errs() << "Processing xar header failed.\n";
    return success;
  }

  xar_close(x);
  std::error_code EC;
  sys::fs::directory_iterator DI = sys::fs::directory_iterator(tmpDir, EC);
  sys::fs::directory_iterator DE;
  auto optionsCopy = options;
  optionsCopy.bitcodeOnly = false;
  for (; !EC && DI != DE; DI = DI.increment(EC)) {
    StringRef path(DI->path());
    success += AnalyzeFileImpl(path, result, optionsCopy);
    sys::fs::remove(path);
  }

  if (std::error_code EC = sys::fs::remove(tmpDir)) {
    errs() << "Could not delete directory" << EC.message() << "\n";
  }
  return 0;
}

int AnalyzeXar(llvm::MemoryBufferRef &fileData,
               APIAnalysisIntermediateResult &result,
               const APIAnalysisOptions &options) {
  int TmpArchiveFD = -1;
  SmallString<128> TmpArchive;

  std::error_code EC = llvm::sys::fs::createTemporaryFile(
      "bitcode.tmp", "xar", TmpArchiveFD, TmpArchive);
  if (EC) {
    errs() << "unable to create temporary file: '" << EC.message() << "'";
    return 1;
  }

  {
    raw_fd_ostream Out(TmpArchiveFD, /*shouldClose=*/true);
    Out << fileData.getBuffer();
  }

  StringRef xarPath = TmpArchive.str();
  auto success = AnalyzeXar(xarPath, result, options);
  sys::fs::remove(xarPath);
  return success;
}