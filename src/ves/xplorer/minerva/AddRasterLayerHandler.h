
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for adding a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_ADD_RASTER_LAYER_HANDLER_H
#define VES_XPLORER_MINERVA_ADD_RASTER_LAYER_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class AddRasterLayerHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  AddRasterLayerHandler();
  virtual ~AddRasterLayerHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_ADD_RASTER_LAYER_HANDLER_H
