#ifndef WAND_SELECTION_H
#define WAND_SELECTION_H
/*!\file WandSelection.h
WandSelection API
*/
/*!\class VE_Xplorer::WandSelection
* 
*/

#include "VE_Xplorer/XplorerHandlers/SelectionHandler.h"
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS WandSelection:public SelectionHandler
   {
      public:
         WandSelection();
         ~WandSelection();

      private:

   };
}

#endif