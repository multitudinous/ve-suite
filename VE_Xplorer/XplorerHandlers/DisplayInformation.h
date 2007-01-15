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
         //Initialize display items
         void InitializeDisplay();

         //Initialize the framerate display
         void InitFrameRateDisplay();

         //Initialize the world coordinate system display
         void InitCoordSysDisplay();

         VE_SceneGraph::cfdSwitch* display_switch;
         VE_SceneGraph::cfdDCS* framerate;
         VE_SceneGraph::cfdFILE* coord_sys;

         bool framerate_flag;                //bool Frame Rate on/off
         bool coord_sys_flag;                //bool WCS on/off
   };
}

#endif //DISPLAY_INFORMATION

#endif //_OSG