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
#include <ves/xplorer/event/cad/CADPhysicsPropertiesEventHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>

#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::open::xml;
using namespace ves::open::xml::cad;

///////////////////////////////////////////////////////////////////////////////////////
CADPhysicsPropertiesEventHandler::CADPhysicsPropertiesEventHandler()
        :
        ves::xplorer::event::CADEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
CADPhysicsPropertiesEventHandler::CADPhysicsPropertiesEventHandler( const CADPhysicsPropertiesEventHandler& rhs )
        :
        ves::xplorer::event::CADEventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CADPhysicsPropertiesEventHandler::~CADPhysicsPropertiesEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CADPhysicsPropertiesEventHandler& CADPhysicsPropertiesEventHandler::operator=( const CADPhysicsPropertiesEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void CADPhysicsPropertiesEventHandler::_operateOnNode( XMLObject* xmlObject )
{
    try
    {
        Command* command = dynamic_cast< Command* >( xmlObject );
        DataValuePairWeakPtr nodeID = command->GetDataValuePair( "Node ID" );
        DataValuePairWeakPtr nodeType = command->GetDataValuePair( "Node Type" );

        if( nodeType->GetDataString() == std::string( "Part" ) )
        {
            double physicsPropertyValue = 0;

            if( command->GetDataValuePair( "Mass" ) )
            {
                command->GetDataValuePair( "Mass" )->GetData( physicsPropertyValue );
                m_cadHandler->GetPart( nodeID->GetDataString() )->GetPhysicsRigidBody()->SetMass( physicsPropertyValue );
            }
            else if( command->GetDataValuePair( "Friction" ) )
            {
                command->GetDataValuePair( "Friction" )->GetData( physicsPropertyValue );
                m_cadHandler->GetPart( nodeID->GetDataString() )->GetPhysicsRigidBody()->setFriction( physicsPropertyValue );
            }
            else if( command->GetDataValuePair( "Restitution" ) )
            {
                command->GetDataValuePair( "Restitution" )->GetData( physicsPropertyValue );
                m_cadHandler->GetPart( nodeID->GetDataString() )->GetPhysicsRigidBody()->setRestitution( physicsPropertyValue );
            }

            std::cout << "Changed Physics Property: " << m_cadHandler->GetPart( nodeID->GetDataString() )->GetFilename() << std::endl;
        }
    }
    catch ( ... )
    {
        std::cout << "Error!!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
