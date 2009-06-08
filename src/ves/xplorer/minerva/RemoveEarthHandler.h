
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for remove earth command.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_REMOVE_EARTH_HANDLER_H
#define VES_XPLORER_MINERVA_REMOVE_EARTH_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class RemoveEarthHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  RemoveEarthHandler();
  virtual ~RemoveEarthHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_REMOVE_EARTH_HANDLER_H
