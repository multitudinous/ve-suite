
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler to navigate to the model.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_NAVIGATE_TO_LAYER_H
#define VES_XPLORER_MINERVA_NAVIGATE_TO_LAYER_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class NavigateToLayer : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  NavigateToLayer();
  virtual ~NavigateToLayer();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_NAVIGATE_TO_LAYER_H
