/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: PluginLoader.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/PluginLoader.h"
#include "VE_Conductor/Framework/Plugin_base.h"

#include <wx/hash.h>
#include <wx/dynload.h>
#include <wx/image.h>
#include <wx/log.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/image.h>
#include <wx/utils.h>

PluginLoader::PluginLoader()
{
  plugins.clear();
  plugin_cls.clear();
  ::wxInitAllImageHandlers();
}

PluginLoader::~PluginLoader()
{
  for ( unsigned int i=0; i<plugins.size(); ++i )
    delete (plugins[i]);

  plugins.clear();
  plugin_cls.clear();
}
    
bool PluginLoader::LoadPlugins(wxString lib_dir)
{
   wxString filename;

   const wxString ext = wxString("*") + wxPluginLibrary::GetDllExt();

   wxLogDebug ("Loading plugins from [%s]\n", lib_dir.c_str());

   /* Create a directory object we can scan for plugins */
   if ( !wxDir::Exists(lib_dir) )
   {
      // deal with the error here - wxDir would already log an error 
      // message explaining the exact reason of the failure
      return FALSE;
   }
   wxDir dir(lib_dir);// + "\\" + "plugins");
   
   if ( !dir.IsOpened() )
   {
      wxString msg(wxString("Directory ") + dir.GetName()+ wxString(" is present but cannot be opened."));
      wxMessageBox( msg,"Plugin Loader Failure", wxOK | wxICON_INFORMATION );
      // deal with the error here - wxDir would already log an error 
      // message explaining the exact reason of the failure
      return FALSE;
   }

   bool cont = dir.GetFirst(&filename, ext, wxDIR_FILES );
   while ( cont )
   {
      wxFileName  libname(lib_dir, filename);
      wxString libn=lib_dir+"/"+libname.GetName();

      wxPluginLibrary *lib = wxPluginManager::LoadLibrary( libn );
      if ( lib )
         wxLogDebug ("Loaded [ %s ]\n", filename.c_str());

      cont = dir.GetNext(&filename);
   }

   wxString veSuiteHome;
   if ( ::wxGetEnv( wxString( "VE_SUITE_HOME" ), &veSuiteHome ) )
   {
      wxString libn = veSuiteHome + "/lib/" + "win32/" + wxString( "DefaultPlugin" ) + wxPluginLibrary::GetDllExt();
      wxPluginLibrary *lib = wxPluginManager::LoadLibrary( libn );
      if ( lib )
         wxLogDebug("Loaded [ %s ]\n", libn );
   }

   RegisterPlugins();

   return TRUE;	
}

void PluginLoader::RegisterPlugins()
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
   node = wxClassInfo::sm_classTable->Next();
   while ( node )
   {
      wxClassInfo *classInfo = (wxClassInfo *)node->GetData();

      if ( wxString( classInfo->GetBaseClassName1() ) == wxString( "REI_Plugin" ) )
      {   
         RegisterPlugin(classInfo);
         wxLogDebug ("|\tRegister plugins : %s",classInfo->GetClassName());
      }
      node = wxClassInfo::sm_classTable->Next();
   }
}

void PluginLoader::RegisterPlugin(wxClassInfo* info)
{
   if (info)
   {
       REI_Plugin* object = (REI_Plugin*)(info->CreateObject());
       plugins.push_back(object);
       plugin_cls.push_back(info);
   }
}
