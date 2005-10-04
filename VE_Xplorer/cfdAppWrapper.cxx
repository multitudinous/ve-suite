/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdAppWrapper.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdAppWrapper.h"

#include "VE_Xplorer/cfdApp.h"
#include "VE_Xplorer/cfdThread.h"
#include "VE_Xplorer/cfdVjObsWrapper.h"

#include <vrj/Kernel/Kernel.h>
#include <vpr/System.h>

#include <iostream>

using namespace VE_Xplorer;

cfdAppWrapper::cfdAppWrapper( int argc,  char* argv[], cfdVjObsWrapper* input )
{
   this->argc = argc;
   this->argv = argv;
   _thread = new cfdThread();
   _vjObsWrapper = input;
   _thread->new_thread=new vpr::Thread(new vpr::ThreadMemberFunctor<cfdAppWrapper>(this, &cfdAppWrapper::init ) );
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

void cfdAppWrapper::init( void * )
{
   vrj::Kernel* kernel = vrj::Kernel::instance(); // Declare a new Kernel
   _cfdApp = new cfdApp();  // Delcare an instance of my application
   _cfdApp->SetWrapper( _vjObsWrapper );
   for ( int i = 1; i < argc; i++ )          // Configure the kernel
   {
      kernel->loadConfigFile( argv[i] );  
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
