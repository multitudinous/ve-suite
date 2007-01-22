//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//Don't implement this class; it is handled through cfdEnvironmentHandler
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef _OSG

#ifndef DISPLAY_INFORMATION_H
#define DISPLAY_INFORMATION_H
/*!\file DisplayInformation.h
DisplayInformation API
*/
/*!\class VE_Xplorer::DisplayInformation
* 
*/
#include "VE_Installer/include/VEConfig.h"

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdSwitch;
   class cfdFILE;
}

namespace osg
{
   class Projection;
   class MatrixTransform;
   class Geode;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS DisplayInformation
   {
      public:
         DisplayInformation();
         ~DisplayInformation();

         void LatePreFrameUpdate();

         //Appropriate display based on input from DisplayEventHandler
         void FrameRateEvent();
         void CoordSysEvent();

         //Set the display flags
         void SetFrameRateFlag(bool val);
         void SetCoordSysFlag(bool val);

      private:
         void InitializeProjection();

         void InitializeTransformation();

         //Initialize the framerate display
         void InitFrameRateDisplay();

         //Initialize the world coordinate system display
         void InitCoordSysDisplay();

         VE_SceneGraph::cfdSwitch* display_switch;
         VE_SceneGraph::cfdDCS* framerate;
         VE_SceneGraph::cfdDCS* coord_sys;

         bool framerate_flag;                //bool Frame Rate on/off
         bool coord_sys_flag;                //bool WCS on/off
   };
}

#endif //DISPLAY_INFORMATION

#endif //_OSG