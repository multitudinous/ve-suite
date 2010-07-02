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

// --- VE-Suite Includes --- //
#include "AppWrapper.h"
#include "App.h"
#include "VjObsWrapper.h"

#ifdef QT_ON && _DARWIN
#include "CocoaHelper.h"
#endif

// --- VR Juggler Includes --- //
#include <vrj/Kernel/Kernel.h>

#include <vpr/System.h>

#include <boost/bind.hpp>

// --- C/C++ Includes --- //
#include <iostream>

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
    bool enableRTT = false;
    for( int i = 1;i < argc;++i )
    {
        if( std::string( argv[i] ) == std::string( "-VESRTT" ) )
        {
            enableRTT = true;
            std::cout << "Enabling RTT"<< std::endl;
            break;
        }
    }

    //Setup the juggler kernel now
    // block it on another thread
    // Delcare an instance of my application
    m_cfdApp = new App( m_argc, m_argv, enableRTT );
    m_cfdApp->SetWrapper( m_vjObsWrapper );
    
    vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel
#if defined QT_ON && defined _DARWIN
    CocoaInit( m_cfdApp );
#endif
    kernel->start();                          // Start the kernel thread

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
