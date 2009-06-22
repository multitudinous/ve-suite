
///////////////////////////////////////////////////////////////////////////////
//
//  Event handler to navigate to the model.
//
///////////////////////////////////////////////////////////////////////////////

#ifndef VES_XPLORER_MINERVA_NAVIGATE_TO_MODEL_H
#define VES_XPLORER_MINERVA_NAVIGATE_TO_MODEL_H

#include "ves/xplorer/minerva/EventHandler.h"

namespace ves {
namespace xplorer {
namespace minerva {


class NavigateToModel : public EventHandler
{
  typedef EventHandler BaseClass;

public:

  NavigateToModel();
  virtual ~NavigateToModel();

  virtual void Execute ( CommandPtr command, MinervaManager& manager );
};


}
}
}

#endif // VES_XPLORER_MINERVA_NAVIGATE_TO_MODEL_H
