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
#include <ves/xplorer/event/cad/CADSetOpacityEventHandler.h>
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
CADSetOpacityEventHandler::CADSetOpacityEventHandler()
        : ves::xplorer::event::CADEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
CADSetOpacityEventHandler::CADSetOpacityEventHandler( const CADSetOpacityEventHandler& rhs )
        : ves::xplorer::event::CADEventHandler( rhs )
{}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
CADSetOpacityEventHandler::~CADSetOpacityEventHandler()
{}
///Equal operator
////////////////////////////////////////////////////////////////////////////////
CADSetOpacityEventHandler& CADSetOpacityEventHandler::operator=( const CADSetOpacityEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }
    return *this;
}
//////////////////////////////////////////////////////////////////////////
void CADSetOpacityEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    try
    {
        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );
        DataValuePairPtr opacityValue = 
            command->GetDataValuePair( "Opacity Value" );
        DataValuePairPtr transparentFlag = 
            command->GetDataValuePair( "Transparent Value" );
        DataValuePairPtr nodeID = 
            command->GetDataValuePair( "Node ID" );
        std::string id;
        nodeID->GetData( id );

        if( opacityValue )
        {
            double opacity = 1.0;
            opacityValue->GetData( opacity );
            m_cadHandler->UpdateOpacity( id, opacity, false );
        }
        else if( transparentFlag )
        {
            ves::xplorer::scenegraph::CADEntity* temp = 
                m_cadHandler->GetPart( id );
            if( temp )
            {
                unsigned int flag;
                transparentFlag->GetData( flag );
                temp->SetTransparencyFlag( flag );
            }
            else
            {
                std::cout << "Cannot set transparency flag on a group." 
                    << std::endl;
            }
        }
    }
    catch ( ... )
    {
        std::cout << "Error!!" << std::endl;
        std::cout << "---Invalid node specified to toggle!---" << std::endl;
    }
}
