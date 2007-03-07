#ifndef SELECTION_HANDLER_H
#define SELECTION_HANDLER_H
/*!\file SelectionHandler.h
SelectionHandler API
*/
/*!\class VE_XPlorer::SelectionHandler
* 
*/
#include <osg/ref_ptr>

#include "VE_Installer/include/VEConfig.h"

namespace osg
{
   class LineSegment;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS SelectionHandler
   {
      public:
         SelectionHandler();
         ~SelectionHandler();

         virtual osg::ref_ptr< osg::LineSegment > CreateLineSegment() = 0;

         void ActivateSelection();
         void DeactivateSelection();
         void Traverse();

      private:
         bool active;
     
   };
}

#endif