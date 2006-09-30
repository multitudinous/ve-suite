#ifndef MOUSE_SELECTION_H
#define MOUSE_SELECTION_H
/*!\file MouseSelection.h
MouseSelection API
*/
/*!\class VE_XPlorer::MouseSelection
* 
*/
#include <osg/ref_ptr>
#include <osg/Matrix>

#include "VE_Xplorer/XplorerHandlers/SelectionHandler.h"
#include "VE_Installer/include/VEConfig.h"

namespace osg
{
   class LineSegment;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS MouseSelection:public SelectionHandler
   {
      public:
         MouseSelection();
         ~MouseSelection();

         virtual osg::ref_ptr<osg::LineSegment> CreateLineSegment();

         void Update();
         void Reshape(unsigned int width,unsigned int height);
         void NormalizeXY(float x,float y);
         osg::ref_ptr<osg::LineSegment> ProjectNormalizedXYIntoObjectCoordinates(osg::Matrix &projectionMatrix,
                                                                                 osg::Matrix &viewMatrix,
                                                                                 float x,float y);

      private:
         unsigned int ms_width;
         unsigned int ms_height;
         float ms_aspectRatio;
         float ms_normalized_x;
         float ms_normalized_y;
         osg::Matrix ms_projection_matrix;
         osg::Matrix ms_view_matrix;
   };
}

#endif