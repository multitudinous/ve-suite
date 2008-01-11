/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/conductor/UIPluginBase.h>
#include "PluginLoader.h"
#include <ves/conductor/DefaultPlugin/DefaultPlugin.h>

#include <wx/hash.h>
#include <wx/dynload.h>
#include <wx/image.h>
#include <wx/log.h>
#include <wx/dir.h>
#include <wx/filename.h>
#include <wx/image.h>
#include <wx/utils.h>
#include <wx/filefn.h>

using namespace ves::conductor;
using namespace ves::conductor::util;

#include <iostream>
////////////////////////////////////////////////////////////////////////////////
PluginLoader::PluginLoader()
{
    plugins.clear();
    plugin_cls.clear();
    ::wxInitAllImageHandlers();
}
////////////////////////////////////////////////////////////////////////////////
PluginLoader::~PluginLoader()
{
    for( size_t i = 0; i < plugins.size(); ++i )
    {
        delete plugins.at( i );
    }
    plugins.clear();
    
    for( size_t i = 0; i < mPluginNames.size(); ++i )
    {
        wxPluginManager::UnloadLibrary( mPluginNames.at( i ) );
    }
    mPluginNames.clear();

    plugin_cls.clear();
}
////////////////////////////////////////////////////////////////////////////////
bool PluginLoader::LoadPlugins( wxString lib_dir )
{
    // Load the default plugin no matter what
    wxString hostType;
    ::wxGetEnv( wxString( "CFDHOSTTYPE", wxConvUTF8 ), &hostType );

    //Load default plugin into vectors
    plugins.push_back( new DefaultPlugin() );
    plugin_cls.push_back( 0 );

    // Try to laod custom plugins
    const wxString ext = wxString( "*", wxConvUTF8 ) + wxPluginLibrary::GetDllExt();
    lib_dir.Append( _( "/" ) );
    lib_dir.Append( hostType );
    wxLogDebug( _( "Loading plugins from [%s]\n" ), lib_dir.c_str() );
    //std::cout << "Loading plugins from "
    //    << ConvertUnicode( lib_dir.c_str() ) << std::endl;
    // Create a directory object we can scan for plugins
    if( !wxDir::Exists( lib_dir ) )
    {
        // deal with the error here - wxDir would already log an error
        // message explaining the exact reason of the failure
        return FALSE;
    }

    wxDir dir( lib_dir );

    if( !dir.IsOpened() )
    {
        // Dispaly error
        wxString msg( _( "Directory " ) + dir.GetName() +
                      _( " is present but cannot be opened." ) );
        wxMessageBox( msg, _( "Plugin Loader Failure" ),
                      wxOK | wxICON_INFORMATION );
        // deal with the error here - wxDir would already log an error
        // message explaining the exact reason of the failure
        return FALSE;
    }

    wxString filename;
    bool cont = dir.GetFirst( &filename, ext, wxDIR_FILES );
    while( cont )
    {
        //std::cout << "try Loaded " << ConvertUnicode( ext.c_str() ) << " "
        //    << ConvertUnicode( filename.c_str() ) << std::endl;

        wxFileName  libname( lib_dir, filename );
        wxString libn = lib_dir + _( "/" ) + libname.GetName();

        wxPluginLibrary *lib = wxPluginManager::LoadLibrary( libn );
        if( lib )
        {
            wxLogDebug( _( "Loaded [ %s ]\n" ), filename.c_str() );
            //std::cout << "Loaded " << ConvertUnicode( filename.c_str() )
            //    << std::endl;
            mPluginNames.push_back( libn );
        }

        cont = dir.GetNext( &filename );
    }

    RegisterPlugins();

    return TRUE;
}
////////////////////////////////////////////////////////////////////////////////
void PluginLoader::RegisterPlugins()
{
    plugins.clear();
    plugin_cls.clear();

    wxClassInfo::sm_classTable->BeginFind();

    /*
     Rip through the entire classinfo hash

     Looking for classes derived from "Plugin",
     but make sure they aren't actually the Plugin class itself !

     This is a real rip off from the wxModule initialisation code
    */
    wxHashTable_Node* node = wxClassInfo::sm_classTable->Next();
    while( node )
    {
        //This has to be a c style cast for some reason...
        wxClassInfo* classInfo = ( wxClassInfo* )( node->GetData() );

        if( wxString( classInfo->GetBaseClassName1() ) ==
                wxString( "UIPluginBase", wxConvUTF8 ) )
        {
            RegisterPlugin( classInfo );
            wxLogDebug( _( "|\tRegister plugins : %s" ),
                        classInfo->GetClassName() );
            //std::cout << "|\tRegister plugins : "
            //    << ConvertUnicode( classInfo->GetClassName() ) << std::endl;
        }
        node = wxClassInfo::sm_classTable->Next();
    }
}
////////////////////////////////////////////////////////////////////////////////
void PluginLoader::RegisterPlugin( wxClassInfo* info )
{
    if( info )
    {
        UIPluginBase* object = ( UIPluginBase* )( info->CreateObject() );
        plugins.push_back( object );
        plugin_cls.push_back( info );
    }
}
////////////////////////////////////////////////////////////////////////////////
size_t PluginLoader::GetNumberOfPlugins( void )
{
    return plugin_cls.size();
}
////////////////////////////////////////////////////////////////////////////////
std::pair< UIPluginBase*, wxClassInfo* > 
    PluginLoader::GetPluginDataPair( size_t i )
{
    std::pair< UIPluginBase*, wxClassInfo* >
        dataPair( plugins.at( i ), plugin_cls.at( i ) );
    return dataPair;
}
