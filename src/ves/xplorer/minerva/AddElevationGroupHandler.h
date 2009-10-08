
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for adding a elevation group.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_ADD_ELEVATION_GROUP_HANDLER_H
#define VES_XPLORER_MINERVA_ADD_ELEVATION_GROUP_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class AddElevationGroupHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  AddElevationGroupHandler();
  virtual ~AddElevationGroupHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_ADD_ELEVATION_GROUP_HANDLER_H
