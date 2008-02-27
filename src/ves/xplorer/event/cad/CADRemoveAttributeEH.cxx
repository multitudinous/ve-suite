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
#include <ves/xplorer/event/cad/CADRemoveAttributeEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/scenegraph/util/Attribute.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAttribute.h>
#include <iostream>

using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADRemoveAttributeEventHandler::CADRemoveAttributeEventHandler()
        : ves::xplorer::event::CADEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
CADRemoveAttributeEventHandler::CADRemoveAttributeEventHandler( const CADRemoveAttributeEventHandler& rhs )
        : ves::xplorer::event::CADEventHandler( rhs )
{}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADRemoveAttributeEventHandler::~CADRemoveAttributeEventHandler()
{}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADRemoveAttributeEventHandler& CADRemoveAttributeEventHandler::operator=( const CADRemoveAttributeEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }
    return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADRemoveAttributeEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    try
    {
        std::cout << "---Adding attribute to node---" << std::endl;
        std::cout << "CADRemoveAttributeEventHandler." << std::endl;
        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
        DataValuePairPtr nodeID = command->GetDataValuePair( "Node ID" );
        DataValuePairPtr nodeType = command->GetDataValuePair( "Node Type" );
        DataValuePairPtr activeAttribute = command->GetDataValuePair( "Attribute Name" );

        m_cadHandler->RemoveAttributeFromNode( nodeID->GetDataString(),
                                               nodeType->GetDataString(),
                                               activeAttribute->GetDataString() );
    }
    catch ( ... )
    {
        std::cout << "Couldn't add attribute to node!!!" << std::endl;
        std::cout << "CADRemoveAttributeEventHandler." << std::endl;
    }
}
