#ifndef CFD_AVAIL_MODULES_H
#define CFD_AVAIL_MODULES_H
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>
//#include <vector>
//#include "wx/image.h"
//#include "wx/imaglist.h"
//#include "wx/treectrl.h"

#include "VE_Xplorer/cfdVEPluginLoader.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS cfdVEAvail_Modules : public wxObject
   {
      public:
         cfdVEAvail_Modules( void );
         ~cfdVEAvail_Modules( void );
         bool LoadModules(); //Load all the modules from the dlls 
         cfdVEPluginLoader* GetLoader( void );

      protected:
         cfdVEPluginLoader* pl_loader;
   };
}
#endif
