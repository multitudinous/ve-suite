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
#include <VE_Xplorer/XplorerHandlers/CADPhysicsMeshEventHandler.h>
#include <VE_Xplorer/XplorerHandlers/cfdModel.h>
#include <VE_Xplorer/XplorerHandlers/ModelCADHandler.h>

#include <VE_Xplorer/SceneGraph/CADEntity.h>

#include <VE_Open/XML/XMLObject.h>
#include <VE_Open/XML/Command.h>
#include <VE_Open/XML/DataValuePair.h>
#include <VE_Open/XML/CAD/CADNode.h>

#include <iostream>

using namespace VE_EVENTS;

///////////////////////////////////////////////////////////////////////////////////////
CADPhysicsMeshEventHandler::CADPhysicsMeshEventHandler()
:
VE_EVENTS::CADEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
CADPhysicsMeshEventHandler::CADPhysicsMeshEventHandler( const CADPhysicsMeshEventHandler& rhs )
:
VE_EVENTS::CADEventHandler( rhs )
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
CADPhysicsMeshEventHandler::~CADPhysicsMeshEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
CADPhysicsMeshEventHandler& CADPhysicsMeshEventHandler::operator=( const CADPhysicsMeshEventHandler& rhs )
{
    if( this != &rhs )
    {
        VE_EVENTS::CADEventHandler::operator=( rhs );
    }

    return *this;
}
///////////////////////////////////////////////////////////////////////////////////////
void CADPhysicsMeshEventHandler::_operateOnNode( VE_XML::XMLObject* xmlObject )
{
    try
    {
        VE_XML::Command* command = dynamic_cast< VE_XML::Command* >(xmlObject);
        VE_XML::DataValuePairWeakPtr nodeID = command->GetDataValuePair( "Node ID" );
        VE_XML::DataValuePairWeakPtr nodeType = command->GetDataValuePair( "Node Type" );
        VE_XML::DataValuePairWeakPtr meshType = command->GetDataValuePair( "Mesh Type" );

        if( nodeType->GetDataString() == std::string( "Part" ) )
        {
            if( meshType->GetDataString() == std::string( "Bounding Box" ) )
            {
                m_cadHandler->GetPart( nodeID->GetDataString() )->GetPhysicsRigidBody()->BoundingBoxShape();
            }
            else if( meshType->GetDataString() == std::string( "Convex" ) )
            {
                m_cadHandler->GetPart( nodeID->GetDataString() )->GetPhysicsRigidBody()->ConvexShape();
            }
            else if( meshType->GetDataString() == std::string( "Static Concave" ) )
            {
                m_cadHandler->GetPart( nodeID->GetDataString() )->GetPhysicsRigidBody()->StaticConcaveShape();
            }

            std::cout << "Changed Physics Mesh: " << m_cadHandler->GetPart( nodeID->GetDataString() )->GetFilename() << std::endl;
        }
    }
    catch(...)
    {
        std::cout << "Error!!" << std::endl;
    }
}
///////////////////////////////////////////////////////////////////////////////////////
