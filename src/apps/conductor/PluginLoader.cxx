/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
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
    wxPluginManager::CreateManifest();
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

    for( size_t i = 0; i < mPluginLibs.size(); ++i )
    {
        //wxPluginManager::UnloadLibrary( mPluginNames.at( i ) );
        if( mPluginLibs.at( i )->IsLoaded() )
        {
            wxDllType tempDLL = mPluginLibs.at( i )->Detach();
            wxDynamicLibrary::Unload( tempDLL );
            //mPluginNames.at( i )->Unload();
            //wxPluginManager::UnloadLibrary( mPluginNames.at( i ) );
        }
    }
    mPluginNames.clear();
    mPluginLibs.clear();
    wxPluginManager::ClearManifest();

    plugin_cls.clear();
}
////////////////////////////////////////////////////////////////////////////////
bool PluginLoader::LoadPlugins( wxString lib_dir )
{
    wxString pluginsDirStr;
    ::wxGetEnv( wxString( "CONDUCTOR_PLUGINS_DIR", wxConvUTF8 ), &pluginsDirStr );
    wxDir pluginsDir( pluginsDirStr );
    const wxString ext = wxString( "*", wxConvUTF8 ) + wxPluginLibrary::GetDllExt();
    //std::cout << "try Loaded " << ConvertUnicode( ext.c_str() ) << " "
    //    << ConvertUnicode( pluginsDirStr.c_str() ) << std::endl;
    if( !wxDir::Exists( pluginsDirStr ) )
    {
        // deal with the error here - wxDir would already log an error
        // message explaining the exact reason of the failure
        //return FALSE;
    }

    if( !pluginsDir.IsOpened() )
    {
        // Dispaly error
        wxString msg( _( "Directory " ) + pluginsDir.GetName() +
                     _( " is present but cannot be opened." ) );
        //wxMessageBox( msg, _( "Plugin Loader Failure" ),
        //             wxOK | wxICON_INFORMATION );
        wxLogMessage( _( "Loading error [%s]" ), msg.c_str() );
        // deal with the error here - wxDir would already log an error
        // message explaining the exact reason of the failure
        std::cout << " Not Opened" << std::endl;
        //return FALSE;
    }

    //Load default plugins for VE-Suite
    wxString filename;
    {
        bool cont = pluginsDir.GetFirst( &filename, ext, wxDIR_FILES );
        while( cont )
        {
            //std::cout << "try Loaded " << ConvertUnicode( ext.c_str() ) << " "
            //    << ConvertUnicode( filename.c_str() ) << std::endl;
        
            wxFileName  libname( pluginsDirStr, filename );
            wxString libn = pluginsDirStr + _( "/" ) + libname.GetName();
        
            wxPluginLibrary *lib = wxPluginManager::LoadLibrary( libn );
            if( lib )
            {
                wxLogMessage( _( "Loaded [ %s ]" ), libn.c_str() );

                mPluginLibs.push_back( lib );
                mPluginNames.push_back( libn );
            }
            cont = pluginsDir.GetNext( &filename );
        }
    }

    // Load the default plugin no matter what
    wxString hostType;
    wxString hostEnv( "CFDHOSTTYPE", wxConvUTF8 );
    ::wxGetEnv( hostEnv, &hostType );

    // Try to laod custom plugins
    lib_dir.Append( _( "/" ) );
    lib_dir.Append( hostType );
    wxLogMessage( _( "Loading plugins from [%s]" ), lib_dir.c_str() );

    // Create a directory object we can scan for plugins
    if( wxDir::Exists( lib_dir ) )
    {
        // deal with the error here - wxDir would already log an error
        // message explaining the exact reason of the failure
        //return FALSE;
        wxDir dir( lib_dir );

        if( !dir.IsOpened() )
        {
            // Dispaly error
            wxString msg( _( "Directory " ) + dir.GetName() +
                      _( " is present but cannot be opened." ) );
            //wxMessageBox( msg, _( "Plugin Loader Failure" ),
            //          wxOK | wxICON_INFORMATION );
            wxLogMessage( msg );
            // deal with the error here - wxDir would already log an error
            // message explaining the exact reason of the failure
            //return FALSE;
        }

        //Load custom app plugins 
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
                wxLogMessage( _( "Loaded [ %s ]" ), filename.c_str() );
                mPluginLibs.push_back( lib );
                mPluginNames.push_back( libn );
            }

            cont = dir.GetNext( &filename );
        }
    }
    
    RegisterPlugins();

    //Load default plugin into vectors
    //NOTE: DefaultPlugins are being loaded somewhere else
    //so we do not need to load them here. Eventually we need
    //to clean this load process up.
    //plugins.push_back( new DefaultPlugin() );
    //plugin_cls.push_back( 0 );

    return TRUE;
}
////////////////////////////////////////////////////////////////////////////////
void PluginLoader::RegisterPlugins()
{
    plugins.clear();
    plugin_cls.clear();

#if wxCHECK_VERSION( 2, 9, 0 )
    //This code is currently broken and needs to be fixed
    //due to the changes for wxClassInfo in wxWidgets-2.9.0
    //Also, RegisterPlugin() function needs to be uncommented
    for( const wxClassInfo* classInfo = wxClassInfo::GetFirst(); classInfo;
         classInfo = classInfo->GetNext() )
    {
        if( wxString( classInfo->GetBaseClassName1() ) ==
                wxString( "UIPluginBase", wxConvUTF8 ) )
        {
            //RegisterPlugin( classInfo );
            wxLogMessage(
                _( "|\tRegister plugins : %s" ), classInfo->GetClassName() );
        }
    }
#else
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
            wxLogMessage( _( "|\tRegister plugins : %s" ),
                        classInfo->GetClassName() );
        }
        node = wxClassInfo::sm_classTable->Next();
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
void PluginLoader::RegisterPlugin( wxClassInfo* info )
{
    if( info )
    {
        UIPluginBase* object =
#if wxCHECK_VERSION( 2, 9, 0 )
            static_cast< UIPluginBase* >( (info->GetConstructor())() );
#else
            static_cast< UIPluginBase* >( info->m_objectConstructor() );
#endif
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
