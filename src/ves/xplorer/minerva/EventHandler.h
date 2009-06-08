
///////////////////////////////////////////////////////////////////////////////
//
//  Base event handler for Minerva commands.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_EVENT_HANDLER_H
#define VES_XPLORER_MINERVA_EVENT_HANDLER_H

#include <ves/VEConfig.h>
#include <ves/open/xml/CommandPtr.h>

#include <string>

namespace ves {
namespace xplorer {
namespace minerva {

  class MinervaManager;
  class ModelWrapper;

class EventHandler
{
public:

  typedef ves::open::xml::CommandPtr CommandPtr;

  EventHandler();
  virtual ~EventHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager ) = 0;

  ModelWrapper* GetOrCreateModel ( const std::string& guid, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_EVENT_HANDLER_H
