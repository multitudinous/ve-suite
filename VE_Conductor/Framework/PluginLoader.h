#ifndef PLUGINLOADER_H
#define PLUGINLOADER_H
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include <wx/log.h>
#include <wx/dir.h>
#include <wx/filename.h>

#include <vector>

class REI_Plugin;

class PluginLoader 
{
public:
   PluginLoader();
   ~PluginLoader();

   bool LoadPlugins(wxString dir);
   //Load all the dlls in the given dir

   void RegisterPlugins();

   //Instantiate a instance of the plug_in. This instance is not used for any network composition but for information.
   void RegisterPlugin(wxClassInfo* info);


   // private:
   std::vector<REI_Plugin*> plugins;
   //Keep the list of the first intance of each plugin
   std::vector<wxClassInfo*> plugin_cls; 
   //The classinfo obj of the each plugin, will be use to generate more instances
};

#endif
