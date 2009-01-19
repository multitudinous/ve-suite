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
#include <ves/xplorer/event/environment/SetResetStartPositionEventHandler.h>
#include <ves/xplorer/CommandHandler.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/OneDDoubleArray.h>

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler::SetResetStartPositionEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler
::SetResetStartPositionEventHandler( const SetResetStartPositionEventHandler& ceh )
{}
////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler::~SetResetStartPositionEventHandler()
{}
////////////////////////////////////////////////////////////////////////////////
SetResetStartPositionEventHandler&
SetResetStartPositionEventHandler::operator=( const SetResetStartPositionEventHandler& rhs )
{
    if( &rhs != this )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void SetResetStartPositionEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* baseObject )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SetResetStartPositionEventHandler::Execute( const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    try
    {
        CommandPtr command = boost::dynamic_pointer_cast<Command>( veXMLObject );
        if( !command->GetDataValuePair( "SET_START_POSITION" ) )
        {
            return;
        }
        
        CommandPtr viewPointGUIData( new Command() );
        viewPointGUIData->SetCommandName( "START_POSITION" );
        
        DataValuePairPtr quatStartPosition( new DataValuePair());
        OneDDoubleArrayPtr quatData( new OneDDoubleArray( 0 ) );
        osg::Quat quat = ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->getAttitude();
        quatData->AddElementToArray( quat[ 0 ] );
        quatData->AddElementToArray( quat[ 1 ] );
        quatData->AddElementToArray( quat[ 2 ] );
        quatData->AddElementToArray( quat[ 3 ] );
        quatStartPosition->SetData( "QUAT_START_POSITION", quatData );
        viewPointGUIData->AddDataValuePair( quatStartPosition );
        
        DataValuePairPtr positionStartPosition( new DataValuePair() );
        OneDDoubleArrayPtr positionsData( new OneDDoubleArray( 0 ) );
        osg::Vec3d trans = ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->getPosition();
        positionsData->AddElementToArray( trans[ 0 ] );
        positionsData->AddElementToArray( trans[ 1 ] );
        positionsData->AddElementToArray( trans[ 2 ] );
        positionStartPosition->SetData( "POSITION_START_POSITION", positionsData );
        viewPointGUIData->AddDataValuePair( positionStartPosition );
        
        ves::xplorer::CommandHandler::instance()->SetXMLCommand( viewPointGUIData );
        
        std::vector< double > tempPos = positionsData->GetArray();
        ves::xplorer::DeviceHandler::instance()->SetResetWorldPosition( quat, tempPos );
    }
    catch ( ... )
    {
        std::cout << "Error!!" << std::endl;
        std::cout << "SetResetStartPositionEventHandler::_operateOnNode()" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
