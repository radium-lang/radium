#ifndef RADIUM_SUBSYSTEMS_H
#define RADIUM_SUBSYSTEMS_H

#include "Radium/Basic/LLVM.h"
#include "Radium/Basic/LangOptions.h"
#include "Radium/Basic/SourceManager.h"
#include "Radium/Parse/Parser.h"
#include "llvm/ADT/StringRef.h"

namespace Radium {

class ParserUnit {
 public:
  ParserUnit(SourceManager& sm, unsigned buffer_id, const LangOptions& options,
             llvm::StringRef module_name);
  ParserUnit(SourceManager& sm, unsigned buffer_id);
  ParserUnit(SourceManager& sm, unsigned buffer_id, unsigned offset,
             unsigned end_offset);

  ~ParserUnit();

  void parse();

  auto getParser() -> Parser&;
  auto getLangOptions() const -> const LangOptions&;

 private:
  struct Implementation;
  Implementation& impl_;
};

}  // namespace Radium

#endif  // RADIUM_SUBSYSTEMS_H