
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for adding an elevation layer.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_ADD_ELEVATION_LAYER_HANDLER_H
#define VES_XPLORER_MINERVA_ADD_ELEVATION_LAYER_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class AddElevationLayerHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  AddElevationLayerHandler();
  virtual ~AddElevationLayerHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_ADD_ELEVATION_LAYER_HANDLER_H
