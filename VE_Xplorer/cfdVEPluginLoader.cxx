#include "cfdVEPluginLoader.h"
#include <wx/image.h>
#include <wx/object.h>
//IMPLEMENT_DYNAMIC_CLASS(cfdVEPluginLoader, wxObject);


cfdVEPluginLoader::cfdVEPluginLoader()
{
   plugins.clear();
   plugin_cls.clear();
   ::wxInitAllImageHandlers();
   //wxClassInfo::InitializeClasses();
}

cfdVEPluginLoader::~cfdVEPluginLoader()
{
   unsigned int i;
  
   for (i=0; i<plugins.size(); i++)
      delete (plugins[i]);

   plugins.clear();
   plugin_cls.clear();
   //wxClassInfo::CleanUpClasses();
}
    
bool cfdVEPluginLoader::LoadPlugins(wxString lib_dir)
{
  wxString filename;

  wxString ext = "*" + wxDllLoader::GetDllExt();
  
  wxLogDebug ("Loading plugins from [%s]", lib_dir.c_str());
  
  /* Create a directory object we can scan for plugins */
  wxDir dir(lib_dir);// + "\\" + "plugins");
  
  if ( !dir.IsOpened() )
    {
      // deal with the error here - wxDir would already log an error message explaining the exact reason of the failure
      cerr << " ERROR : Can't find plugin dir! " << endl;
      return FALSE;
    }
  wxDynamicLibrary* lib;
	bool cont = dir.GetFirst(&filename, ext, wxDIR_FILES );

	if ( cont )
   {
      while ( cont )
	   {
	      wxFileName  libname(lib_dir, filename);
	      const wxString libn=lib_dir+"/"+libname.GetName()+wxDllLoader::GetDllExt();
         lib = new wxDynamicLibrary( libn );
	      //wxPluginLibrary *lib = wxPluginManager::LoadLibrary (libn);
         if (lib->IsLoaded())
	      {
            wxLogDebug ("Loaded [ %s ]", filename.c_str());
	      }
         else
         {
            wxLogDebug ("Could Not Load [ %s ]", filename.c_str());
         }
         cont = dir.GetNext(&filename);
     
         //delete lib;
	   }

      RegisterPlugins();
      return TRUE;	
	}
   else
	{
      wxLogDebug ("Appropriate directory is present but no plugins are present" );
      return false;	      
   }
}

void cfdVEPluginLoader::RegisterPlugins()
{
   wxNode *node;
  
   plugins.clear();
   plugin_cls.clear();
   const wxChar *name = "cfdVEBaseClass";
   wxClassInfo *lastInfo = NULL;
   for ( wxClassInfo *info = wxClassInfo::sm_first; info; info = info->m_next )
   {
      //cout << info << " : " << info->m_className << endl;
      if (wxStrcmp(info->m_baseClassName1, name) == 0)
      {
         cfdVEBaseClass *object = (cfdVEBaseClass *) info->CreateObject();
         plugins.push_back(object);
         plugin_cls.push_back(info);
         cout << info->m_className << endl;
      }

      if ( info->m_next == wxClassInfo::sm_first )
         break;
      if ( lastInfo != info  )
         lastInfo = info;
      else
         break;
   }
//   wxClassInfo::sm_classTable->BeginFind();
  
   /*
    Rip through the entire classinfo hash
    
    Looking for classes derived from "Plugin",
    but make sure they aren't actually the Plugin class itself !
    
    This is a real rip off from the wxModule initialisation code
   */
/*  
   while ( (node = wxClassInfo::sm_classTable->Next()) )
   {
      wxClassInfo *classInfo = (wxClassInfo *)node->Data();
      if ( classInfo->IsKindOf(CLASSINFO(cfdVEBaseClass)) &&
	   (classInfo != (& (cfdVEBaseClass::sm_classcfdVEBaseClass))) )
	   {
         cout << classInfo->GetClassName() << endl;
	      RegisterPlugin(classInfo);
	   }
   }*/
}

void cfdVEPluginLoader::RegisterPlugin(wxClassInfo* info)
{
   //int id;
/*cout << " register plugins" << endl;
   if (info)
     {
       cfdVEBaseClass *object = (cfdVEBaseClass *) info->CreateObject();
       plugins.push_back(object);
       plugin_cls.push_back(info);
       cout << info->GetClassName() << endl;
     }*/
}

char* cfdVEPluginLoader::GetPluginName( int index )
{

   char* _name;// = plugins.at(index)->GetName();
   _name = NULL;
   return _name;
}

int cfdVEPluginLoader::GetNumberOfPlugins( void )
{
   return plugins.size();
}

cfdVEBaseClass* cfdVEPluginLoader::CreateObject( char* _objname )
{
   int selectPlugin = -1;

   for (unsigned int i=0; i<plugins.size(); i++)
   {  
      
      if ( plugins.at(i)->GetName() == _objname )
      {
         selectPlugin = i;
         break;
      }
   }

   if (selectPlugin == -1)
   {
      cerr <<"ERROR: cfdVEPluginLoader::CreateObject : " << _objname 
            << " : Plugin Not Found!"<<endl;
      return NULL;
   }

   return (cfdVEBaseClass*)plugin_cls.at( selectPlugin )->CreateObject();
}
