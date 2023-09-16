#include "Radium/Subsystems.h"

namespace Radium {

struct ParserUnit::Implementation {
  LangOptions lang_opts;
  ASTContext& ctx;
  std::unique_ptr<Parser> the_parser;

  Implementation(SourceManager& sm, unsigned buffer_id,
                 const LangOptions& lang_opts, llvm::StringRef module_name)
      : lang_opts(lang_opts) {}
};


} // namespace Radium