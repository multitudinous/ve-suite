#include "cfdVEPluginLoader.h"
#include <wx/image.h>

IMPLEMENT_DYNAMIC_CLASS(cfdVEPluginLoader, wxObject);

#include <wx/image.h>

cfdVEPluginLoader::cfdVEPluginLoader()
{
  plugins.clear();
  plugin_cls.clear();
  ::wxInitAllImageHandlers();
}

cfdVEPluginLoader::~cfdVEPluginLoader()
{
  unsigned int i;
  
  for (i=0; i<plugins.size(); i++)
    delete (plugins[i]);

  plugins.clear();
  plugin_cls.clear();
}
    
bool cfdVEPluginLoader::LoadPlugins(wxString lib_dir)
{
  wxString filename;

  wxString ext = "*" + wxDllLoader::GetDllExt();
  
  wxLogDebug ("Loading plugins from [%s]\n", lib_dir.c_str());
  
  /* Create a directory object we can scan for plugins */
  wxDir dir(lib_dir);// + "\\" + "plugins");
  
  if ( !dir.IsOpened() )
    {
      // deal with the error here - wxDir would already log an error message explaining the exact reason of the failure
      return FALSE;
    }
  
	bool cont = dir.GetFirst(&filename, ext, wxDIR_FILES );
	while ( cont )
	{
	  wxFileName  libname(lib_dir, filename);
	  wxString libn=lib_dir+"/"+libname.GetName();
	  wxPluginLibrary *lib = wxPluginManager::LoadLibrary (libn);
	  if (lib)
	    wxLogDebug ("Loaded [ %s ]\n", filename.c_str());
	  cont = dir.GetNext(&filename);
	}

	RegisterPlugins();
	
	return TRUE;	
}

void cfdVEPluginLoader::RegisterPlugins()
{
  wxNode *node;
  
  plugins.clear();
  plugin_cls.clear();

  wxClassInfo::sm_classTable->BeginFind();
  
  /*
    Rip through the entire classinfo hash
    
    Looking for classes derived from "Plugin",
    but make sure they aren't actually the Plugin class itself !
    
    This is a real rip off from the wxModule initialisation code
  */
  
  while ( (node = wxClassInfo::sm_classTable->Next()) )
    {
      wxClassInfo *classInfo = (wxClassInfo *)node->Data();
      
      if ( classInfo->IsKindOf(CLASSINFO(cfdVEBaseClass)) &&
	   (classInfo != (& (cfdVEBaseClass::sm_classcfdVEBaseClass))) )
	   {
	      RegisterPlugin(classInfo);
	   }
    }
}

void cfdVEPluginLoader::RegisterPlugin(wxClassInfo* info)
{
   //int id;

   if (info)
     {
       cfdVEBaseClass *object = (cfdVEBaseClass *) info->CreateObject();
       plugins.push_back(object);
       plugin_cls.push_back(info);
     }
}

char* cfdVEPluginLoader::GetPluginName( int index )
{
   char* _name;// = plugins.at(index)->GetName();
   return _name;
}

int cfdVEPluginLoader::GetNumberOfPlugins( void )
{
   return plugins.size();
}

cfdVEBaseClass* cfdVEPluginLoader::CreateObject( char* _objname )
{
   int selectPlugin = -1;

   for (int i=0; i<plugins.size(); i++)
   {  
      if ( plugins.at(i)->GetName() == _objname )
      {
         selectPlugin = i;
         break;
      }
   }

   if (selectPlugin == -1)
   {
      cerr<<"ERROR: cfdVEPluginLoader::CreateObject : Plugin Not Found!"<<endl;
      return NULL;
   }

   return (cfdVEBaseClass*)plugin_cls.at( selectPlugin )->CreateObject();
}
