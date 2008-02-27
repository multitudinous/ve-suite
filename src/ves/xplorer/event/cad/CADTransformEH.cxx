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
#include <ves/xplorer/event/cad/CADTransformEH.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelCADHandler.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/Clone.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/open/xml/Transform.h>
#include <ves/open/xml/TransformPtr.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/cad/CADNode.h>
#include <iostream>
using namespace ves::xplorer::event;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CADTransformEventHandler::CADTransformEventHandler()
        : ves::xplorer::event::CADEventHandler()
{}
///////////////////////////////////////////////////////////////////////////////////////
CADTransformEventHandler::CADTransformEventHandler( const CADTransformEventHandler& rhs )
        : ves::xplorer::event::CADEventHandler( rhs )
{}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CADTransformEventHandler::~CADTransformEventHandler()
{}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CADTransformEventHandler& CADTransformEventHandler::operator=( const CADTransformEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::CADEventHandler::operator=( rhs );
    }
    return *this;
}
///////////////////////////////////////////////////////////////////////
void CADTransformEventHandler::_operateOnNode( XMLObjectPtr xmlObject )
{
    try
    {

        CommandPtr command( boost::dynamic_pointer_cast<ves::open::xml::Command>( xmlObject ) );

        DataValuePairPtr nodeID = command->GetDataValuePair( "Node ID" );
        DataValuePairPtr nodeType = command->GetDataValuePair( "Node Type" );
        DataValuePairPtr newTransform = command->GetDataValuePair( "Transform" );

        ves::xplorer::scenegraph::DCS* transform = 0;

        if( nodeType->GetDataString() == std::string( "Part" ) )
        {
            if( m_cadHandler->PartExists( nodeID->GetDataString() ) )
            {
                transform = m_cadHandler->GetPart( nodeID->GetDataString() )->GetDCS();
            }
        }
        else if( nodeType->GetDataString() == std::string( "Assembly" ) )
        {
            if( m_cadHandler->AssemblyExists( nodeID->GetDataString() ) )
            {
                transform = m_cadHandler->GetAssembly( nodeID->GetDataString() );
            }
        }
        /*else if( nodeType->GetDataString() == std::string( "Clone" ) )
        {
            if( m_cadHandler->CloneExists( nodeID->GetDataString() ) )
            {
                transform = m_cadHandler->GetClone( nodeID->GetDataString() )->GetClonedGraph();
            }
        }*/

        if( transform )
        {
            TransformPtr rawTransform( boost::dynamic_pointer_cast<Transform>( newTransform->GetDataXMLObject() ) );
            transform->SetTranslationArray( rawTransform->GetTranslationArray()->GetArray() );
            transform->SetRotationArray( rawTransform->GetRotationArray()->GetArray() );
            transform->SetScaleArray( rawTransform->GetScaleArray()->GetArray() );
        }
    }
    catch ( ... )
    {
        std::cout << "Error!!!Invalid command passed to CADTransformEH!!" << std::endl;
    }
}
