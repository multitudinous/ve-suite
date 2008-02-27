/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/event/cad/CADDeleteNodeEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Clone.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>
#include <iostream>
using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADDeleteNodeEventHandler::CADDeleteNodeEventHandler()
        : ves::xplorer::event::CADEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
CADDeleteNodeEventHandler::CADDeleteNodeEventHandler( const CADDeleteNodeEventHandler& rhs )
        : ves::xplorer::event::CADEventHandler( rhs )
{}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADDeleteNodeEventHandler::~CADDeleteNodeEventHandler()
{}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADDeleteNodeEventHandler& CADDeleteNodeEventHandler::operator=( const CADDeleteNodeEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }
    return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADDeleteNodeEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    try
    {
        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
        DataValuePairPtr parentID = command->GetDataValuePair( "Parent ID" );
        DataValuePairPtr nodeID = command->GetDataValuePair( "Node ID" );
        DataValuePairPtr nodeType = command->GetDataValuePair( "Node Type" );

        //ves::xplorer::Model* activeModel = dynamic_cast<ves::xplorer::Model*>(_baseObject);
        std::cout << "---Deleting node---" << std::endl;
        ves::xplorer::scenegraph::DCS* parentAssembly = 0;
        parentAssembly = m_cadHandler->GetAssembly( parentID->GetDataString() );

        //This assumes the part/assembly isn't there already
        if( nodeType->GetDataString() == std::string( "Assembly" ) )
        {
            parentAssembly->RemoveChild( m_cadHandler->GetAssembly( nodeID->GetDataString() ) );
        }
        else if( nodeType->GetDataString() == std::string( "Part" ) )
        {
            parentAssembly->RemoveChild( m_cadHandler->GetPart( nodeID->GetDataString() )->GetDCS() );
        }
        else if( nodeType->GetDataString() == std::string( "Clone" ) )
        {
            parentAssembly->RemoveChild( m_cadHandler->GetClone( nodeID->GetDataString() )->GetClonedGraph() );
        }
        //Need to also remove the node from ModelCADHandler node maps
        m_cadHandler->RemoveNode( nodeID->GetDataString(), nodeType->GetDataString() );
    }
    catch ( ... )
    {
        std::cout << "Error!!" << std::endl;
        std::cout << "---Invalid node specified to remove!---" << std::endl;
    }
}
