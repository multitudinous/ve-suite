
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

namespace Minerva { namespace Core { namespace Layers { class RasterLayer; } } }

namespace ves {
namespace xplorer {
namespace minerva {

  class MinervaManager;
  class ModelWrapper;

class EventHandler
{
public:

  typedef ves::open::xml::CommandPtr CommandPtr;
  typedef Minerva::Core::Layers::RasterLayer RasterLayer;

  EventHandler();
  virtual ~EventHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager ) = 0;

protected:

  ModelWrapper* GetOrCreateModel ( const std::string& guid, MinervaManager& manager );

  static RasterLayer* _createRasterLayerFromCommand ( CommandPtr command );
};


}
}
}

#endif // VES_XPLORER_MINERVA_EVENT_HANDLER_H
