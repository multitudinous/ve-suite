
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for removing an elevation layer.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_REMOVE_ELEVATION_LAYER_HANDLER_H
#define VES_XPLORER_MINERVA_REMOVE_ELEVATION_LAYER_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class RemoveElevationLayerHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  RemoveElevationLayerHandler();
  virtual ~RemoveElevationLayerHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_REMOVE_ELEVATION_LAYER_HANDLER_H
