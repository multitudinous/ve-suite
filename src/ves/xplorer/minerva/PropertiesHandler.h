
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for set geographic properties command.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_PROPERTIES_HANDLER_H
#define VES_XPLORER_MINERVA_PROPERTIES_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class PropertiesHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  PropertiesHandler();
  virtual ~PropertiesHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_PROPERTIES_HANDLER_H
