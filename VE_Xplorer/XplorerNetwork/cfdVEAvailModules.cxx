#include "VE_Xplorer/XplorerNetwork/cfdVEAvailModules.h"
#include <iostream>

#include <fstream>
#include <sstream>

using namespace VE_Xplorer;

cfdVEAvail_Modules::cfdVEAvail_Modules( void )
{
   pl_loader = new cfdVEPluginLoader();
   LoadModules();
}

cfdVEAvail_Modules::~cfdVEAvail_Modules( void )
{
   delete pl_loader;
}

bool cfdVEAvail_Modules::LoadModules()
{
/*   char* path = "Plugins/";
   char* modelPath =  getenv("CFDHOSTTYPE");
   char* file = new char[100];
   
   strcpy( file, path );
   strcat( file, modelPath );

   wxString wxPath = file;
   pl_loader->LoadPlugins( wxPath );*/
   pl_loader->ScanAndLoad();
  
   //delete [] file;

   return true;
}

cfdVEPluginLoader* cfdVEAvail_Modules::GetLoader( void )
{
   return pl_loader;
}

