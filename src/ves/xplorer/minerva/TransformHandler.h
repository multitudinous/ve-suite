
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler for cad transform.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_TRANSFORM_HANDLER_H
#define VES_XPLORER_MINERVA_TRANSFORM_HANDLER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class TransformHandler : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  TransformHandler();
  virtual ~TransformHandler();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_TRANSFORM_HANDLER_H
