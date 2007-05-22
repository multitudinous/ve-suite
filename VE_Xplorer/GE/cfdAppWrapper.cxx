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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/GE/cfdAppWrapper.h"

#include "VE_Xplorer/GE/cfdApp.h"
#include "VE_Xplorer/XplorerHandlers/cfdThread.h"
#include "VE_Xplorer/GE/cfdVjObsWrapper.h"

#include <vrj/Kernel/Kernel.h>

#include <vpr/System.h>

#include <boost/bind.hpp>

#include <iostream>

using namespace VE_Xplorer;

cfdAppWrapper::cfdAppWrapper( int argc,  char* argv[], cfdVjObsWrapper* input )
{
   this->argc = argc;
   this->argv = argv;
   _thread = new cfdThread();
   _vjObsWrapper = input;
#if __VJ_version > 2000003
   _thread->new_thread=new vpr::Thread( boost::bind(&cfdAppWrapper::init, this) );
#elif __VJ_version == 2000003
   _thread->new_thread=new vpr::Thread( new vpr::ThreadMemberFunctor< cfdAppWrapper >( this, &cfdAppWrapper::init ) );
#endif
   jugglerIsRunning = true;
}

cfdAppWrapper::~cfdAppWrapper( void )
{
   if ( _thread )
   {
	  // _thread->new_thread->kill();
      delete _thread;
   }
}

bool cfdAppWrapper::JugglerIsRunning( void )
{
   return jugglerIsRunning;
}

#if __VJ_version > 2000003
void cfdAppWrapper::init( void )
#elif __VJ_version == 2000003
void cfdAppWrapper::init( void* )
#endif
{
   vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel
   _cfdApp = new cfdApp( argc, argv );  // Delcare an instance of my application
   _cfdApp->SetWrapper( _vjObsWrapper );
#if __VJ_version >= 2003000
   kernel->init(argc, argv);
#elif __VJ_version == 2000003
#endif
   for ( int i = 1; i < argc; i++ )          // Configure the kernel
   {
      if ( std::string( argv[ i ] ) == std::string( "-VESDesktop" ) )
      {
         i = i + 2;
      }
      else if ( std::string( argv[ i ] ) == std::string( "-VESCluster" ) )
      {
         //do nothing
         ; 
      }
      else
      {
         kernel->loadConfigFile( argv[i] );  
      }
   }
   kernel->start();                          // Start the kernel thread

   kernel->setApplication( _cfdApp );    // Give application to kernel
   
   kernel->waitForKernelStop();              // Block until kernel stops

   delete this->_cfdApp;
   this->_cfdApp = NULL;

   delete this->_vjObsWrapper;
	this->_vjObsWrapper = NULL;
   jugglerIsRunning = false;
}
