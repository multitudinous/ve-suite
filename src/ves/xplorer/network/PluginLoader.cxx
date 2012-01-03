/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/network/PluginLoader.h>
#include <ves/xplorer/plugin/PluginBase.h>
#include <ves/xplorer/Debug.h>
#include <ves/open/xml/model/Model.h>
#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>

#include <iostream>

#include <vpr/vpr.h>
#include <vpr/System.h>

#if defined(VPR_OS_Windows)
static const std::string DSO_SUFFIX( ".dll" );
#elif defined(VPR_OS_Darwin)
static const std::string DSO_SUFFIX( ".bundle" );
#else
static const std::string DSO_SUFFIX( ".so" );
#endif

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/version.hpp>

using namespace ves::xplorer;
using namespace ves::xplorer::plugin;
using namespace ves::xplorer::network;
////////////////////////////////////////////////////////////////////////////////
PluginLoader::PluginLoader()
{
    plugins.clear();

    ves::xplorer::eventmanager::EventManager::instance()->
            RegisterSignal( new ves::xplorer::eventmanager::SignalWrapper
                            < createPluginSignal_type >( &m_createUIPlugin ),
            "PluginLoader.CreatePlugin" );
}
////////////////////////////////////////////////////////////////////////////////
PluginLoader::~PluginLoader()
{
    std::map< int, PluginBase* >::iterator iter;
    for( iter = plugins.begin(); iter != plugins.end(); ++iter )
    {
        delete iter->second;
    }
    plugins.clear();

    for( size_t i = 0; i < libs.size(); i++ )
    {
        if( libs.at( i )->isLoaded() )
        {
            try
            {
                libs.at( i )->unload();
            }
            catch(...)
            {
                ;
            }
        }
    }
    libs.clear();
}
////////////////////////////////////////////////////////////////////////////////
int PluginLoader::GetNumberOfPlugins( void )
{
    return plugins.size();
}
////////////////////////////////////////////////////////////////////////////////
PluginBase* PluginLoader::CreateObject( std::string _objname )
{
    int selectPlugin = -1;
    std::map< int, PluginBase* >::iterator iter;
    for( iter = plugins.begin(); iter != plugins.end(); ++iter )
    {
        if( iter->second->GetName() == _objname )
        {
            selectPlugin = iter->first;
            vprDEBUG( vesDBG, 1 )  << "|\tCreating plugin " 
                << _objname << std::endl << vprDEBUG_FLUSH;
            break;
        }
    }

    if( selectPlugin == -1 )
    {
        std::cerr << "|\tPluginLoader::CreateObject : " << _objname
        << " : Plugin Not Found." << std::endl;
        return NULL;
    }

    PluginBase* result = CreateNewPlugin( selectPlugin );

    m_createUIPlugin( _objname, result );

    return result;
}
////////////////////////////////////////////////////////////////////////////////
void PluginLoader::ScanAndLoad( void )
{
    //Get the path for the plugins loaded for vesuite
    std::string vesuitePath;
    bool vesuiteHomeDefined = false;
    vpr::System::getenv( std::string("XPLORER_PLUGINS_DIR"), vesuitePath );
    vprDEBUG(vesDBG,0) << "|\tSearching XPLORER_PLUGINS_DIR for VES Plugins." 
        << std::endl 
        << vprDEBUG_FLUSH;
    //Look for VE-Suite default plugin path
    try
    {
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
        boost::filesystem::path vesuiteDirPath( 
            vesuitePath );
#else
        boost::filesystem::path vesuiteDirPath( 
            vesuitePath, boost::filesystem::no_check );
#endif
        if( boost::filesystem::is_directory( vesuiteDirPath ) )
        {
            vesuiteHomeDefined = true;
        }
    }
    catch( const std::exception& ex )
    {
        vprDEBUG( vesDBG, 1 ) << ex.what()
            << std::endl
            << vprDEBUG_FLUSH;
    }        

    //Look for custom plugin path
    std::string modelPath;
    //vpr::System::getenv( std::string( "CFDHOSTTYPE" ), modelPath );
    std::string path( "Plugins/GE/" );
    std::string libDir = path + modelPath;
    bool customPlugins = false;
    try
    {
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
        boost::filesystem::path dir_path( libDir );
#else
        boost::filesystem::path dir_path( libDir, boost::filesystem::no_check );
#endif
        if( boost::filesystem::is_directory( dir_path ) )
        {
            for( boost::filesystem::recursive_directory_iterator itr( dir_path ); 
                itr !=  boost::filesystem::recursive_directory_iterator(); ++itr )
            {
                if( boost::filesystem::is_directory( itr->status() ) )
                {
                    vpr::LibraryFinder finder( itr->path().string(), DSO_SUFFIX );
                    
                    vpr::LibraryFinder::LibraryList tempLibs;
                    tempLibs = finder.getLibraries();
                    vprDEBUG( vesDBG, 1 )  << "|\tNumber of user custom libs : "
                        << tempLibs.size() << " " << itr->path()
                        << " " << DSO_SUFFIX << std::endl
                        << vprDEBUG_FLUSH;
                    if( tempLibs.size() > 0 )
                    {
                        libDir = itr->path().string();
                        customPlugins = true;
                        break;
                    }
                }
            }
        }
    }
    catch( const std::exception& ex )
    {
        vprDEBUG( vesDBG, 1 ) << ex.what()
        << std::endl
        << vprDEBUG_FLUSH;
        //This is a hack because somehow the if statement above
        //is true in some cases on windows.
        customPlugins = false;
    }

    // Load the custon plugin
    if( customPlugins )
    {
        vpr::LibraryFinder finder( libDir, DSO_SUFFIX );

        libs = finder.getLibraries();
        vprDEBUG( vesDBG, 1 )  << "|\tNumber of user custom libs : "
            << libs.size()
            << " " << DSO_SUFFIX << std::endl
            << vprDEBUG_FLUSH;
    }

    //Load ves default plugins
    if( vesuiteHomeDefined )
    {
        vpr::LibraryFinder::LibraryList defaultLibs;
        vpr::LibraryFinder finder( vesuitePath, DSO_SUFFIX );
        defaultLibs = finder.getLibraries();

        vprDEBUG( vesDBG, 1 )  << "|\tNumber of VE-Suite libs : "
            << defaultLibs.size()
            << " " << DSO_SUFFIX << std::endl
            << vprDEBUG_FLUSH;
        // Load the default plugin
         for(size_t i = 0; i < defaultLibs.size(); ++i )
         {
             vpr::LibraryPtr tempPtr = defaultLibs.at( i );
#ifdef WIN32
             //This is a hack for windows because without this here
             //xplorer will crash. I am unsure of why this is the case
             //and it could not be determined through significant debugging.
             //If anyone can shed light on the problem a fix here would be
             //appreciated.
             Sleep(1000);
#endif
             libs.push_back( tempPtr );
         }
    }

    for( size_t i = 0; i < libs.size(); ++i )
    {
        try
        {
            libs.at( i )->load();
            vprDEBUG( vesDBG, 1 ) << "|\tLoaded lib "
                << libs.at( i )->getName() << " successfully."
                << std::endl << vprDEBUG_FLUSH;
        }
        catch( ... )
        {
            vprDEBUG( vesDBG, 1 ) << "|\tLoaded lib failed : "
                << std::endl << vprDEBUG_FLUSH;
        }
    }

    LoadPlugins();
}
////////////////////////////////////////////////////////////////////////////////
void PluginLoader::LoadPlugins( void )
{
    for( size_t i = 0; i < libs.size(); ++i )
    {
        PluginBase* test_obj( 0 );

        if( libs.at( i )->isLoaded() )
        {
            test_obj = CreateNewPlugin( i );
        }

        if( test_obj )
        {
            plugins[ i ] = test_obj;
            std::cout << "|\tLoaded and created plugin "
                << test_obj->GetName() << std::endl;
        }
        else
        {
            std::cout << "|\tCould not load plugin "
                << i << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
PluginBase* PluginLoader::CreateNewPlugin( unsigned int input )
{
    void*( *creator )();
    PluginBase* test_obj( NULL );

    //This came from vpr test code
    creator = ( void * ( * )() ) libs[ input ]->findSymbol( std::string( "CreateVEPlugin" ) );
    if( NULL != creator )
    {
        vprDEBUG( vesDBG, 1 )  << "|\tCreated plugin " << std::endl << vprDEBUG_FLUSH;
    }
    else
    {
        return 0;
    }

    void* object = ( *creator )();
    if( NULL != object )
    {
        vprDEBUG( vesDBG, 1 )  <<  "|\tCreated object instance " << std::endl << vprDEBUG_FLUSH;
    }

    // Is there a way to test that this cast was successful?
    test_obj = static_cast<PluginBase*>( object );
    return test_obj;
}
////////////////////////////////////////////////////////////////////////////////
