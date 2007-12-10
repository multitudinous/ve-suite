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
#include "AppWrapper.h"

#include "App.h"
#include <ves/xplorer/Thread.h>
#include "VjObsWrapper.h"

#include <vrj/Kernel/Kernel.h>

#include <vpr/System.h>

#include <boost/bind.hpp>

#include <iostream>

using namespace ves::xplorer;
////////////////////////////////////////////////////////////////////////////////
AppWrapper::AppWrapper( int argc,  char* argv[], VjObsWrapper* input ):
        m_argc( argc ),
        m_argv( argv ),
        m_vjObsWrapper( input ),
        m_thread( 0 ),
        m_jugglerIsRunning( false )
{
    //Setup the juggler kernel now
    // block it on another thread
    // Delcare an instance of my application
    m_cfdApp = new App( m_argc, m_argv );
    m_cfdApp->SetWrapper( m_vjObsWrapper );

    vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel
    kernel->start();                          // Start the kernel thread

    kernel->setApplication( m_cfdApp );    // Give application to kernel

    /*m_thread = new Thread();
    #if __VJ_version > 2000003
    m_thread->new_thread = 
        new vpr::Thread( boost::bind(&AppWrapper::init, this) );
    #elif __VJ_version == 2000003
    m_thread->new_thread = 
        new vpr::Thread( new vpr::ThreadMemberFunctor< AppWrapper >( this, &AppWrapper::init ) );
    #endif
    m_jugglerIsRunning = true;*/
}
////////////////////////////////////////////////////////////////////////////////
AppWrapper::~AppWrapper( void )
{
    if( m_thread )
    {
        delete m_thread;
    }

    delete m_cfdApp;
    m_cfdApp = NULL;

    delete m_vjObsWrapper;
    m_vjObsWrapper = NULL;
    m_jugglerIsRunning = false;
}
////////////////////////////////////////////////////////////////////////////////
bool AppWrapper::JugglerIsRunning( void )
{
    return m_jugglerIsRunning;
}
////////////////////////////////////////////////////////////////////////////////
#if __VJ_version > 2000003
void AppWrapper::init( void )
#elif __VJ_version == 2000003
void AppWrapper::init( void* )
#endif
{
    //vrj::Kernel::instance()->doWaitForKernelStop();// Block until kernel stops

    delete m_cfdApp;
    m_cfdApp = NULL;

    delete m_vjObsWrapper;
    m_vjObsWrapper = NULL;
    m_jugglerIsRunning = false;
}
////////////////////////////////////////////////////////////////////////////////
