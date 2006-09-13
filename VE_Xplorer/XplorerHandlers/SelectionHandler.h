#ifndef SELECTION_HANDLER_H
#define SELECTION_HANDLER_H

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

         virtual osg::ref_ptr<osg::LineSegment> CreateLineSegment()=0;

         void ActivateSelection();
         void DeactivateSelection();
         void SelectObjects();

      private:
         bool active;
     
   };
}

#endif