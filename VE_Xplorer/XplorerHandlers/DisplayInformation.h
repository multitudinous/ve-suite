#ifndef DISPLAY_INFORMATION_H
#define DISPLAY_INFORMATION_H
/*!\file DisplayInformation.h
DisplayInformation API
*/
/*!\class VE_Xplorer::DisplayInformation
* 
*/
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/Switch.h"

#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/CameraNode>
#include <osgText/Text>
#endif

//C/C++ Libraries
#include <vector>

namespace VE_SceneGraph
{
	class Switch;
	class CADEntity;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS DisplayInformation
   {
      public:
         DisplayInformation();
         ~DisplayInformation();

         void LatePreFrame();

         ///Set the display flags
         void SetFrameRateFlag( bool val );
         void SetCoordSysFlag( bool val );

			void SetTextColor( std::vector< double > color );

			void SetDisplayPositions( unsigned int width, unsigned int height );

      private:
         ///Initialize the framerate display
         void InitFrameRateDisplay();

         ///Initialize the world coordinate system display
         void InitCoordSysDisplay();

			osg::ref_ptr< VE_SceneGraph::Switch > display_switch;

			osg::ref_ptr< osg::CameraNode > framerate;
			osg::ref_ptr< osg::CameraNode > wcs;

			osg::ref_ptr< osgText::Text > framerate_text;
			osg::ref_ptr< osgText::Text > wcs_x_text;
			osg::ref_ptr< osgText::Text > wcs_y_text;
			osg::ref_ptr< osgText::Text > wcs_z_text;

			VE_SceneGraph::CADEntity* wcs_model;
   };
}

#endif //DISPLAY_INFORMATION
