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
#include <ves/xplorer/event/environment/PhysicsSimulationEventHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/CharacterController.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

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

using namespace ves::xplorer::event;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler::PhysicsSimulationEventHandler()
    :
    ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler::PhysicsSimulationEventHandler(
    const PhysicsSimulationEventHandler& rhs )
    :
    ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler::~PhysicsSimulationEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
PhysicsSimulationEventHandler& PhysicsSimulationEventHandler::operator=(
    const PhysicsSimulationEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulationEventHandler::SetGlobalBaseObject(
    ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulationEventHandler::Execute(
    const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    ves::open::xml::CommandPtr command =
        boost::dynamic_pointer_cast< ves::open::xml::Command >( veXMLObject );

    if( command->GetDataValuePair( "CharacterControllerOn" ) )
    {
        vxs::CharacterController* characterController =
            vxs::SceneManager::instance()->GetCharacterController();
        characterController->TurnOn();
    }
    else if( command->GetDataValuePair( "CharacterControllerOff" ) )
    {
        vxs::CharacterController* characterController =
            vxs::SceneManager::instance()->GetCharacterController();
        characterController->TurnOff();
    }
    else if( command->GetDataValuePair( "ResetPhysicsSimulation" ) )
    {
        vxs::PhysicsSimulator::instance()->SetIdle( true );
        vxs::PhysicsSimulator::instance()->ResetScene();
    }
    else if( command->GetDataValuePair( "PausePhysicsSimulation" ) )
    {
        vxs::PhysicsSimulator::instance()->SetIdle( true );
    }
    else if( command->GetDataValuePair( "StartPhysicsSimulation" ) )
    {
        vxs::PhysicsSimulator::instance()->SetIdle( false );
    }
    else if( command->GetDataValuePair( "StepPhysicsSimulation" ) )
    {
        vxs::PhysicsSimulator::instance()->SetIdle( true );
        vxs::PhysicsSimulator::instance()->StepSimulation();
    }
    else if( command->GetDataValuePair( "Physics Debugger Toggle Value" ) )
    {
        unsigned int toggle = 0;
        command->GetDataValuePair( "Physics Debugger Toggle Value" )->
            GetData( toggle );
        vxs::PhysicsSimulator::instance()->SetDebuggingOn( toggle );
    }
}
////////////////////////////////////////////////////////////////////////////////
