/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: cfdVEPluginLoader.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/cfdVEPluginLoader.h"
#include "VE_Xplorer/cfdVEBaseClass.h"
#include <iostream>

#include <wx/dir.h>
#include <wx/filename.h>

using namespace VE_Xplorer;
IMPLEMENT_APP(cfdVEPluginLoader);

cfdVEPluginLoader::cfdVEPluginLoader()
{
   plugins.clear();
   plugin_cls.clear();
   //::wxInitAllImageHandlers();
   wxPluginLibrary::ms_classes = new wxDLImports;
   wxPluginManager::CreateManifest();
   //wxClassInfo::InitializeClasses();
   //SetTopWindow( NULL );
}

cfdVEPluginLoader::~cfdVEPluginLoader()
{
   for (unsigned int i=0; i<plugins.size(); i++)
   {
      delete (plugins[i]);
   }
   for (unsigned int i=0; i<libs.size(); i++)
   {
   delete libs.at(i);
   }
   plugins.clear();
   plugin_cls.clear();
   libs.clear();
   delete wxPluginLibrary::ms_classes;
   wxPluginLibrary::ms_classes = NULL;
   wxPluginManager::ClearManifest();
   wxClassInfo::CleanUp();
   wxModule::CleanUpModules();
   //this->OnExit();
   this->CleanUp();
}
    
bool cfdVEPluginLoader::LoadPlugins(wxString lib_dir)
{
   wxString filename;

   const wxString ext = wxString("*") + wxPluginLibrary::GetDllExt();
  
   wxLogDebug ("Loading plugins from [%s]", lib_dir.c_str());
  
   /* Create a directory object we can scan for plugins */
   wxDir dir(lib_dir);// + "\\" + "plugins");
  
   if ( !dir.IsOpened() )
   {
      // deal with the error here - wxDir would already log an 
      // error message explaining the exact reason of the failure
      std::cerr << "|\tCan't find plugin dir. " << std::endl;
      return FALSE;
   }

   bool cont = dir.GetFirst(&filename, ext, wxDIR_FILES );

   if ( cont )
   {
      while ( cont )
      {
         wxFileName  libname(lib_dir, filename);
         const wxString libn=lib_dir+"/"+libname.GetName()+wxPluginLibrary::GetDllExt();
         //libs.push_back( new wxDynamicLibrary( libn ) );
         libs.push_back( wxPluginManager::LoadLibrary (libn) );

         if (libs.back()->IsLoaded())
         {
            wxLogDebug ("Loaded [ %s ]", filename.c_str());
         }
         else
         {
            wxLogDebug ("Could Not Load [ %s ]", filename.c_str());
            //wxClassInfo* test = CLASSINFO(lib->GetLibHandle() )
            //cout << test->m_className << endl;
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

   wxClassInfo::sm_classTable->BeginFind();
  
   /*
    Rip through the entire classinfo hash
    
    Looking for classes derived from "Plugin",
    but make sure they aren't actually the Plugin class itself !
    
    This is a real rip off from the wxModule initialisation code
   */
  
   while ( (node = wxClassInfo::sm_classTable->Next()) )
   {
      wxClassInfo *classInfo = (wxClassInfo *)node->GetData();
		if ( wxString( classInfo->GetBaseClassName1() ) == wxString( "VE_Xplorer::cfdVEBaseClass" ) )
	   {   
         cfdVEBaseClass* object = (cfdVEBaseClass *) classInfo->CreateObject();
         plugins.push_back(object);
         plugin_cls.push_back(classInfo);
         std::cout << "|\tRegister plugins : " << classInfo->GetClassName() << std::endl;
      }
   }
}

/*char* cfdVEPluginLoader::GetPluginName( int index )
{
   char* _name;// = plugins.at(index)->GetName();
   _name = NULL;
   index
   return _name;
}*/

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
      std::cerr <<"ERROR: cfdVEPluginLoader::CreateObject : " << _objname 
            << " : Plugin Not Found!"<< std::endl;
      return NULL;
   }

   return (cfdVEBaseClass*)((wxClassInfo*)plugin_cls.at( selectPlugin ))->CreateObject();
}


bool cfdVEPluginLoader::OnInit()
{

  SetAppName("VE-Conductor");
   
   SetTopWindow(NULL);
   return true;

}
