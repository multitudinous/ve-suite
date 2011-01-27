/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

// --- VE-Suite Includes --- //
#include "AppWrapper.h"
#include "App.h"
#include "VjObsWrapper.h"

#include <ves/xplorer/environment/cfdDisplaySettings.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#if defined _DARWIN
#include "CocoaHelper.h"
#endif

// --- VR Juggler Includes --- //
#include <vrj/Kernel/Kernel.h>

#include <vpr/System.h>

#include <boost/bind.hpp>

// --- C/C++ Includes --- //
#include <iostream>

#include <osgDB/FileUtils>

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
AppWrapper::AppWrapper( int argc,  char* argv[], VjObsWrapper* input )
    :
    m_cfdApp( 0 ),
    m_jugglerIsRunning( false ),
    m_vjObsWrapper( input ),
    m_argc( argc ),
    m_argv( argv )
{
    SetupOSGFILEPATH();
    
    int desktopWidth = 0;
    int desktopHeight = 0;
    bool enableRTT = false;
    bool desktopMode = false;
    for( int i = 1;i < argc;++i )
    {
        if( std::string( argv[i] ) == std::string( "-VESRTT" ) )
        {
            enableRTT = true;
            std::cout << "|\tEnabling RTT"<< std::endl;
        }
        else if( ( std::string( argv[ i ] ) == std::string( "-VESDesktop" ) ) && 
           ( argc > i + 2 ) )
        {
            desktopWidth = atoi( argv[ i + 1 ] );
            desktopHeight = atoi( argv[ i + 2 ] );
            desktopMode = true;
        }
    }

    //Setup the juggler kernel now
    // block it on another thread
    // Delcare an instance of my application
    m_cfdApp = new App( m_argc, m_argv, enableRTT );
    m_cfdApp->SetWrapper( m_vjObsWrapper );
    
    vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel
#if defined _DARWIN
    CocoaInit( m_cfdApp );
#endif
    kernel->start();                          // Start the kernel thread

    //We could ask to resize our window here based on command line args.
    //After we call start the application has handed the config files
    //off to jccl::ConfigManager.
    if( (desktopWidth > 0) && (desktopHeight > 0) && desktopMode )
    {
        vpr::System::msleep( 50 );  // 2 thenth-second delay
        cfdDisplaySettings* displaySettings = new cfdDisplaySettings();

        std::cout << 
            "| Initializing....................................  Desktop Display |" 
            << std::endl;
        // Create the command and data value pairs
        // to adjust the desktop settings.
        ves::open::xml::DataValuePairPtr dvpDesktopWidth( new ves::open::xml::DataValuePair( std::string( "FLOAT" ) ) );
        dvpDesktopWidth->SetDataName( "desktop_width" );
        dvpDesktopWidth->SetDataValue( static_cast< double >( desktopWidth ) );
        
        ves::open::xml::DataValuePairPtr dvpDesktopHeight( new ves::open::xml::DataValuePair( std::string( "FLOAT" ) ) );
        dvpDesktopHeight->SetDataName( "desktop_height" );
        dvpDesktopHeight->SetDataValue( static_cast< double >( desktopHeight ) );
        
        ves::open::xml::CommandPtr displayCommand( new ves::open::xml::Command() );
        displayCommand->SetCommandName( std::string( "Juggler_Desktop_Data" ) );
        displayCommand->AddDataValuePair( dvpDesktopWidth );
        displayCommand->AddDataValuePair( dvpDesktopHeight );
        displaySettings->SetVECommand( displayCommand );
        displaySettings->ProcessCommand();
        delete displaySettings;
    }    

    kernel->setApplication( m_cfdApp );    // Give application to kernel

    //vpr::Thread* thread = 
    //    new vpr::Thread( boost::bind(&AppWrapper::init, this) );
    //m_jugglerIsRunning = true;
}
////////////////////////////////////////////////////////////////////////////////
AppWrapper::~AppWrapper()
{
    delete m_cfdApp;
    m_cfdApp = NULL;

    delete m_vjObsWrapper;
    m_vjObsWrapper = NULL;
    m_jugglerIsRunning = false;
}
////////////////////////////////////////////////////////////////////////////////
bool AppWrapper::JugglerIsRunning()
{
    return m_jugglerIsRunning;
}
////////////////////////////////////////////////////////////////////////////////
void AppWrapper::init()
{
    vrj::Kernel::instance()->waitForKernelStop();// Block until kernel stops
/*
    delete m_cfdApp;
    m_cfdApp = NULL;

    delete m_vjObsWrapper;
    m_vjObsWrapper = NULL;
    m_jugglerIsRunning = false;*/
}
////////////////////////////////////////////////////////////////////////////////
void AppWrapper::SetupOSGFILEPATH()
{
    osgDB::FilePathList& fileList = osgDB::getDataFilePathList();
    /*for( size_t i = 0; i < fileList.size(); ++i )
    {
        std::cout << fileList.at( i ) << std::endl;
    }*/

    std::string xplorerBaseDir;
    vpr::System::getenv( "XPLORER_BASE_DIR", xplorerBaseDir );
    std::string vesDir = xplorerBaseDir + "/share/vesuite";
    std::string glslDir = vesDir + "/glsl";
	std::string bdfxDir = vesDir + "/bdfx-data";
    fileList.push_back( vesDir );
    fileList.push_back( glslDir );
	fileList.push_back( bdfxDir );
}
////////////////////////////////////////////////////////////////////////////////
