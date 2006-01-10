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
 * File:          $RCSfile: CorbaManager.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <CorbaManager.h>
#include <iostream>

#include "cfdSteadyStateVizHandler.h"
#include "cfdEnvironmentHandler.h"
#include "cfdModelHandler.h"
#include "cfdCommandArray.h"

#include <vpr/Util/Debug.h>
#include "cfdThread.h"
#include "cfdVjObsWrapper.h"

using namespace std;
//-------------------------------------------------------------
//CorbaManager methods.
//-------------------------------------------------------------

//Constructor
CorbaManager::CorbaManager()
{
   _vjObs = new cfdVjObsWrapper();
   _thread = new cfdThread();
	start();
}

CorbaManager::~CorbaManager()
{
}

cfdCommandArray* CorbaManager::GetCommandArray( void )
{
   return _vjObs->GetCommandArray();
}

void CorbaManager::GetCfdStateVariables( void )
{
   _vjObs->GetCfdStateVariables();
}

void CorbaManager::PreFrameUpdate( void )
{
   _vjObs->PreFrameUpdate();
}

/*void CorbaManager::GetUpdateClusterStateVariables( void )
{
   _vjObs->GetUpdateClusterStateVariables();
}*/

void CorbaManager::SetHandlers( cfdSteadyStateVizHandler* _steadystateHandler, 
                           cfdEnvironmentHandler* _environmentHandler, 
                           cfdModelHandler* _modelHandler )
{
   _vjObs->SetHandlers( _steadystateHandler, _environmentHandler, _modelHandler );
}

double CorbaManager::GetShortArray( int i )
{
   return _vjObs->GetShortArray( i );
}

//init() Get CORBA ready to go
void CorbaManager::init( void * )
{
   cout << "here q " <<endl;
   _vjObs->init();
}

//start() Create a thread to start the server in
void CorbaManager::start()
{
	//_thread->corba_run=new vpr::ThreadMemberFunctor<CorbaManager>(this, &CorbaManager::init );
	//_thread->new_thread=new vpr::Thread(_thread->corba_run);
   //this->init( NULL );
}
