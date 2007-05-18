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
 * Date modified: $Date: 2007-05-09 08:53:08 -0500 (Wed, 09 May 2007) $
 * Version:       $Rev: 7574 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: PhysicsSimulationEventHandler.cxx 7574 2007-05-09 13:53:08Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Xplorer/XplorerHandlers/PhysicsSimulationEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"

#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

// --- VR Juggler Includes --- //
#include <boost/filesystem/operations.hpp>   
#include <boost/filesystem/path.hpp>

// --- C/C++ Libraries --- //
#include <string>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace VE_EVENTS;

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler::PhysicsSimulationEventHandler()
:
VE_EVENTS::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler::PhysicsSimulationEventHandler( const PhysicsSimulationEventHandler& rhs )
:
VE_EVENTS::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler::~PhysicsSimulationEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler& PhysicsSimulationEventHandler::operator=( const PhysicsSimulationEventHandler& rhs )
{
    if( this != &rhs )
    {
        VE_EVENTS::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulationEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulationEventHandler::Execute( VE_XML::XMLObject* veXMLObject )
{
    VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
  
    if( command->GetDataValuePair( "ResetPhysicsSimulation" ) )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->SetIdle( true );
        VE_SceneGraph::PhysicsSimulator::instance()->ResetScene();
    }
    else if( command->GetDataValuePair( "PausePhysicsSimulation" ) )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->SetIdle( true );
    }
    else if( command->GetDataValuePair( "StartPhysicsSimulation" ) )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->SetIdle( false );
    }
    else if( command->GetDataValuePair( "StepPhysicsSimulation" ) )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->SetIdle( true );
        VE_SceneGraph::PhysicsSimulator::instance()->StepSimulation();
    }
}
////////////////////////////////////////////////////////////////////////////////
