
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for add earth command.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_ADD_EARTH_HANDLER_H
#define VES_XPLORER_MINERVA_ADD_EARTH_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class AddEarthHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  AddEarthHandler();
  virtual ~AddEarthHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_ADD_EARTH_HANDLER_H
