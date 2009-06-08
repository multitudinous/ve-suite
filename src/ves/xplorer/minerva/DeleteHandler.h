
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for deleting.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_DELETE_HANDLER_H
#define VES_XPLORER_MINERVA_DELETE_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class DeleteHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  DeleteHandler();
  virtual ~DeleteHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_DELETE_HANDLER_H
