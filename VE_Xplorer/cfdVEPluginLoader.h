#ifndef CFD_PLUGINLOADER_H
#define CFD_PLUGINLOADER_H
#include <wx/wx.h>
#include <wx/log.h>
#include <wx/dynlib.h>
#include <wx/dir.h>
#include <wx/filename.h>

#include <vector>
#include "cfdVEBaseClass.h"

/*
 * If we're using wx in Dynamic Library format do we 
 * want FL to be in DLL form as well?
 */
#if defined(WXUSINGDLL) && \
    (defined(WXMAKING_PLUGIN_DLL) || defined(WXUSING_PLUGIN_DLL))

#if defined(WXMAKING_PLUGIN_DLL)
    // When building the DLL WXPLUGINDECLSPEC exports classes
#   define WXPLUGIN_DECLSPEC            WXEXPORT
#elif defined(WXUSING_PLUGIN_DLL)
    // When building the DLL WXPLUGINDECLSPEC imports classes
#   define WXPLUGIN_DECLSPEC            WXIMPORT
#endif // defined(WXBUILD_PLUGIN_DLL)

#else
// When building the static library nullify the effect of WXPLUGIN_DECLSPEC
#define WXPLUGIN_DECLSPEC
#endif // WXUSINGDLL && (WXMAKING_PLUGIN_DLL || WXUSING_PLUGIN_DLL)


class cfdVEPluginLoader : public wxObject
{
  //DECLARE_DYNAMIC_CLASS(cfdVEPluginLoader)

    public:

  cfdVEPluginLoader();
  ~cfdVEPluginLoader();
  
  bool LoadPlugins(wxString dir);
  //Load all the dlls in the given dir

  void RegisterPlugins();

  //Instantiate a instance of the plug_in. This instance is not used for any network composition but for information.
  void RegisterPlugin(wxClassInfo* info);

  char* GetPluginName(int);

  int GetNumberOfPlugins();

  cfdVEBaseClass* CreateObject( char* ); 
  // private:
  vector<cfdVEBaseClass*> plugins;
  //Keep the list of the first intance of each plugin
  vector<wxClassInfo*> plugin_cls; 
  //The classinfo obj of the each plugin, will be use to generate more instances
  

};

#endif
