#ifndef CFD_AVAIL_MODULES_H
#define CFD_AVAIL_MODULES_H

#include "VE_Xplorer/XplorerNetwork/cfdVEPluginLoader.h"

namespace VE_Xplorer
{
class cfdVEAvail_Modules
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
