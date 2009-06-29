
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for removing a raster layer.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_REMOVE_RASTER_LAYER_HANDLER_H
#define VES_XPLORER_MINERVA_REMOVE_RASTER_LAYER_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class RemoveRasterLayerHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  RemoveRasterLayerHandler();
  virtual ~RemoveRasterLayerHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_REMOVE_RASTER_LAYER_HANDLER_H
