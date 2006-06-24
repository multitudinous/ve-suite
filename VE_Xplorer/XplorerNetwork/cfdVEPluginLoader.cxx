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
 * File:          $RCSfile: cfdVEPluginLoader.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/XplorerNetwork/cfdVEPluginLoader.h"
#include "VE_Xplorer/GraphicalPlugin/cfdVEBaseClass.h"
#include "VE_Xplorer/cfdDebug.h"

#include <iostream>

#include <vpr/vpr.h>
#include <vpr/System.h>

#if defined(VPR_OS_Windows)
static const std::string DSO_SUFFIX(".dll");
#elif defined(VPR_OS_Darwin)
static const std::string DSO_SUFFIX(".dylib");
#else
static const std::string DSO_SUFFIX(".so");
#endif

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace VE_Xplorer;

cfdVEPluginLoader::cfdVEPluginLoader()
{
   plugins.clear();
}
//////////////////////////////////////////////////////////////////
cfdVEPluginLoader::~cfdVEPluginLoader()
{
   std::map< int, cfdVEBaseClass* >::iterator iter;
   for ( iter = plugins.begin(); iter != plugins.end(); ++iter)
   {
      delete iter->second;
   }

   for ( size_t i=0; i<libs.size(); i++)
   {
      if ( libs.at(i)->isLoaded() )
         libs.at(i)->unload();
      //delete libs.at(i);
   }
   plugins.clear();
   //plugin_cls.clear();
   libs.clear();
}
//////////////////////////////////////////////////////////////////
int cfdVEPluginLoader::GetNumberOfPlugins( void )
{
   return plugins.size();
}
//////////////////////////////////////////////////////////////////
cfdVEBaseClass* cfdVEPluginLoader::CreateObject( std::string _objname )
{
   int selectPlugin = -1;
   std::map< int, cfdVEBaseClass* >::iterator iter;
   for ( iter = plugins.begin(); iter != plugins.end(); ++iter )
   {
      if ( iter->second->GetName() == _objname )
      {
         selectPlugin = iter->first;
         break;
      }
   }

   if ( selectPlugin == -1 )
   {
      std::cerr <<"|\tcfdVEPluginLoader::CreateObject : " << _objname 
            << " : Plugin Not Found."<< std::endl;
      return NULL;
   }

   return CreateNewPlugin( selectPlugin );
}
//////////////////////////////////////////////////////////////////
void cfdVEPluginLoader::ScanAndLoad( void )
{
   //Look for custom plugin path
   std::string path("Plugins/GE/");
   std::string modelPath;
   vpr::ReturnStatus status = vpr::System::getenv( std::string("CFDHOSTTYPE"), modelPath );
   std::string libDir = path + modelPath;

   //std::string modelPath;
   std::string vesuitePath;
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
   }

   std::string vesuiteLibDir = vesuitePath + path + libDir;
   const std::string nameCheck( "native" );
   bool customPlugins = false;
   try
   {
      boost::filesystem::path dir_path( libDir );
	   //boost::filesystem::path vesuiteDirPath( vesuiteLibDir, boost::filesystem::no_check );
      boost::filesystem::is_directory( dir_path );
      customPlugins = true;
   }
   catch ( const std::exception& ex )
   {
      vprDEBUG(vesDBG,1) << ex.what() 
                           << std::endl 
                           << vprDEBUG_FLUSH;
      
   }
   
   // Load the custon plugin
   if ( customPlugins )
   {
      vpr::LibraryFinder finder(libDir, DSO_SUFFIX);

      libs = finder.getLibraries();
      vprDEBUG(vesDBG,1)  << " Number of libs : " 
                           << libs.size() 
                           << " " << DSO_SUFFIX << std::endl 
                           << vprDEBUG_FLUSH;
   }

   // Load the default plugin
   vpr::LibraryFinder finder(vesuiteLibDir, DSO_SUFFIX);

   vpr::LibraryFinder::LibraryList defaultLibs = finder.getLibraries();
   vprDEBUG(vesDBG,1)  << " Number of libs : " 
                        << libs.size() 
                        << " " << DSO_SUFFIX << std::endl 
                        << vprDEBUG_FLUSH;
   for ( size_t i = 0; i < defaultLibs.size(); ++i )
   {
      libs.push_back( defaultLibs.at( i ) );
   }

   for ( size_t i = 0; i < libs.size(); ++i )
   {
      status = libs.at( i )->load();
      vprDEBUG(vesDBG,1)  << " Loaded lib successfully : " 
                           << status.success() 
                           << std::endl 
                           << vprDEBUG_FLUSH;
   }
   
   LoadPlugins();
}
//////////////////////////////////////////////////////////////////
void cfdVEPluginLoader::LoadPlugins( void )
{
   for ( size_t i = 0; i < libs.size(); ++i )
   {
      cfdVEBaseClass* test_obj( 0 );

      if ( libs.at( i )->isLoaded() )
      {
         test_obj = CreateNewPlugin( i );
      }

      if ( test_obj )
      {
         plugins[ i ] = test_obj;
         std::cout << "|\tLoaded and created plugin " 
                     << test_obj->GetName() << std::endl;
      }
   }
}
//////////////////////////////////////////////////////////////////
cfdVEBaseClass* cfdVEPluginLoader::CreateNewPlugin( unsigned int input )
{
   void* (*creator)();
   cfdVEBaseClass* test_obj( NULL );

   // No, *this* is the weirdest cast I have ever written.
   creator = (void*(*)()) libs[ input ]->findSymbol( std::string("CreateVEPlugin") );
   if ( NULL != creator )
   {
      vprDEBUG(vesDBG,1)  << " created plugin " << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      return 0;
   }

   void* object = (*creator)();
   if ( NULL != object )
      vprDEBUG(vesDBG,1)  <<  " created Object creation " << std::endl << vprDEBUG_FLUSH;

   // Is there a way to test that this cast was successful?
   test_obj = static_cast<cfdVEBaseClass*>(object);
   return test_obj;
}
