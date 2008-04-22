/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/network/cfdVEPluginLoader.h>
#include <ves/xplorer/plugin/PluginBase.h>
#include <ves/xplorer/Debug.h>
#include <ves/open/xml/model/Model.h>
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

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace ves::xplorer;
using namespace ves::xplorer::plugin;
using namespace ves::xplorer::network;
////////////////////////////////////////////////////////////////////////////////
cfdVEPluginLoader::cfdVEPluginLoader()
{
    plugins.clear();
}
////////////////////////////////////////////////////////////////////////////////
cfdVEPluginLoader::~cfdVEPluginLoader()
{
    std::map< int, PluginBase* >::iterator iter;
    for( iter = plugins.begin(); iter != plugins.end(); ++iter )
    {
        delete iter->second;
    }
    plugins.clear();

#ifndef WIN32
//This define is required because on windows when dll's are unloaded the
//memory allocated by the dll's is also unloaded. This causes problems with
//OSG because OSG has ref_ptr's still pointing to memory allocated by the dll
//a few frames AFTER the dll has been unloaded. This is not a problem
//on other operating systems due to the way plugable modules are handled
//by the operating system. For more information on this bug please see
//the thread on the osg list by Paul Martz on April 19th, 2008
//titled: Clearing RenderLeaf ref_ptrs
//If a better solution for managing this problem can be created it should be
//investigated as this solution is a major hack.
    for( size_t i = 0; i < libs.size(); i++ )
    {
        if( libs.at( i )->isLoaded() )
        {
            libs.at( i )->unload();
        }
    }
#endif
    libs.clear();
}
////////////////////////////////////////////////////////////////////////////////
int cfdVEPluginLoader::GetNumberOfPlugins( void )
{
    return plugins.size();
}
//////////////////////////////////////////////////////////////////
PluginBase* cfdVEPluginLoader::CreateObject( std::string _objname )
{
    int selectPlugin = -1;
    std::map< int, PluginBase* >::iterator iter;
    for( iter = plugins.begin(); iter != plugins.end(); ++iter )
    {
        if( iter->second->GetName() == _objname )
        {
            selectPlugin = iter->first;
            break;
        }
    }

    if( selectPlugin == -1 )
    {
        std::cerr << "|\tcfdVEPluginLoader::CreateObject : " << _objname
        << " : Plugin Not Found." << std::endl;
        return NULL;
    }

    return CreateNewPlugin( selectPlugin );
}
//////////////////////////////////////////////////////////////////
void cfdVEPluginLoader::ScanAndLoad( void )
{
    //Look for custom plugin path
    std::string path( "Plugins/GE/" );
    std::string modelPath;
    vpr::System::getenv( std::string( "CFDHOSTTYPE" ), modelPath );
    std::string libDir = path + modelPath;

    //std::string modelPath;
    /*std::string vesuitePath;
    bool vesuiteHomeDefined = false;
    //status = vpr::System::getenv( std::string("CFDHOSTTYPE"), modelPath );
    if(vpr::System::getenv( std::string("VE_SUITE_HOME"), vesuitePath ).success())
    {
       vprDEBUG(vesDBG,0) << "Searching VE_SUITE_HOME for Default Plugin" 
                            << std::endl 
                            << vprDEBUG_FLUSH;
       //Look for VE-Suite default plugin path
       path.assign("/bin/");
       vesuiteHomeDefined = true;
    }
    else if(vpr::System::getenv( std::string("VE_INSTALL_DIR"), vesuitePath ).success())
    {
       vprDEBUG(vesDBG,0) << "Searching VE_INSTALL_DIR for Default Plugin" 
                            << std::endl 
                            << vprDEBUG_FLUSH;
       //Look for VE-Suite default plugin path
       path.assign("/bin/");
       vesuiteHomeDefined = false;
    }*/

    const std::string nameCheck( "native" );
    bool customPlugins = false;
    try
    {
        boost::filesystem::path dir_path( libDir, boost::filesystem::no_check );
        //boost::filesystem::path vesuiteDirPath( vesuiteLibDir, boost::filesystem::no_check );
        if( boost::filesystem::is_directory( dir_path ) )
        {
            customPlugins = true;
        }
    }
    catch( const std::exception& ex )
    {
        vprDEBUG( vesDBG, 1 ) << ex.what()
        << std::endl
        << vprDEBUG_FLUSH;
    }

    // Load the custon plugin
    if( customPlugins )
    {
        vpr::LibraryFinder finder( libDir, DSO_SUFFIX );

        libs = finder.getLibraries();
        vprDEBUG( vesDBG, 1 )  << "|\tNumber of libs : "
            << libs.size()
            << " " << DSO_SUFFIX << std::endl
            << vprDEBUG_FLUSH;
    }

    // Load the default plugin
    /*std::string vesuiteLibDir = vesuitePath + path + libDir;
    vpr::LibraryFinder finder(vesuiteLibDir, DSO_SUFFIX);

    vpr::LibraryFinder::LibraryList defaultLibs = finder.getLibraries();
    vprDEBUG(vesDBG,1)  << " Number of libs : " 
                         << libs.size() 
                         << " " << DSO_SUFFIX << std::endl 
                         << vprDEBUG_FLUSH;
    for(size_t i = 0; i < defaultLibs.size(); ++i )
    {
       libs.push_back( defaultLibs.at( i ) );
    }*/

    for( size_t i = 0; i < libs.size(); ++i )
    {
        try
        {
            libs.at( i )->load();
            vprDEBUG( vesDBG, 1 ) << "|\tLoaded lib successfully : "
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
//////////////////////////////////////////////////////////////////
void cfdVEPluginLoader::LoadPlugins( void )
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
//////////////////////////////////////////////////////////////////
PluginBase* cfdVEPluginLoader::CreateNewPlugin( unsigned int input )
{
    //std::cout << " CreateNewPlugin plugin " << input << std::endl;
    void*( *creator )();
    PluginBase* test_obj( NULL );

    // No, *this* is the weirdest cast I have ever written.
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
        vprDEBUG( vesDBG, 1 )  <<  "|\tCreated object instance " << std::endl << vprDEBUG_FLUSH;

    // Is there a way to test that this cast was successful?
    test_obj = static_cast<PluginBase*>( object );
    return test_obj;
}
