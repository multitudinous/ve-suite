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
#include <ves/xplorer/event/cad/CADInitializePhysicsEventHandler.h>
#include <ves/xplorer/cfdModel.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>

#include <iostream>

using namespace VE_EVENTS;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;

///////////////////////////////////////////////////////////////////////////////////////
CADInitializePhysicsEventHandler::CADInitializePhysicsEventHandler()
:
VE_EVENTS::CADEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
CADInitializePhysicsEventHandler::CADInitializePhysicsEventHandler( const CADInitializePhysicsEventHandler& rhs )
:
VE_EVENTS::CADEventHandler( rhs )
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
CADInitializePhysicsEventHandler::~CADInitializePhysicsEventHandler()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////////////
CADInitializePhysicsEventHandler& CADInitializePhysicsEventHandler::operator=( const CADInitializePhysicsEventHandler& rhs )
{
    if( this != &rhs )
    {
        VE_EVENTS::CADEventHandler::operator=( rhs );
    }

    return *this;
}
///////////////////////////////////////////////////////////////////////////////////////
void CADInitializePhysicsEventHandler::_operateOnNode( XMLObject* xmlObject )
{
    try
    {
        Command* command = dynamic_cast< Command* >(xmlObject);
        DataValuePairWeakPtr nodeID = command->GetDataValuePair( "Node ID" );
        DataValuePairWeakPtr nodeType = command->GetDataValuePair( "Node Type" );

        if( nodeType->GetDataString() == std::string( "Part" ) )
        {
            std::cout << "Initialized Physics: " << m_cadHandler->GetPart( nodeID->GetDataString() )->GetFilename() << std::endl;
            m_cadHandler->GetPart( nodeID->GetDataString() )->InitPhysics();
        }
    }
    catch(...)
    {
        std::cout << "Error!!" << std::endl;
    }
}
///////////////////////////////////////////////////////////////////////////////////////
