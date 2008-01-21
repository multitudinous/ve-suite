/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <ves/xplorer/event/environment/NavigationDataEventHandler.h>

#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/OneDDoubleArray.h>

#include <boost/filesystem/operations.hpp>   //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <string>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace ves::xplorer::event;
using namespace ves::xplorer;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
NavigationDataEventHandler::NavigationDataEventHandler()
        : ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NavigationDataEventHandler::NavigationDataEventHandler( const NavigationDataEventHandler& rhs )
        : ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
NavigationDataEventHandler::~NavigationDataEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationDataEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler )
{
    _baseObject = modelHandler;
}
////////////////////////////////////////////////////////////////////////////////
void NavigationDataEventHandler::Execute( XMLObject* veXMLObject )
{
    Command* command = dynamic_cast<Command*>( veXMLObject );
    dynamic_cast< Device* >( _baseObject )->SetVECommand( command );

    DataValuePairWeakPtr quatPosition = command->GetDataValuePair( "QUAT_START_POSITION" );
    if( quatPosition )
    {
        OneDDoubleArray* data = dynamic_cast< OneDDoubleArray* >( quatPosition->GetDataXMLObject() );
        std::vector< double > tempQuat = data->GetArray();
        osg::Quat quat( tempQuat[ 0 ], tempQuat[ 1 ], tempQuat[ 2 ], tempQuat[ 3 ] );
        ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->SetQuat( quat );

        quatPosition = command->GetDataValuePair( "POSITION_START_POSITION" );
        data = dynamic_cast< OneDDoubleArray* >( quatPosition->GetDataXMLObject() );
        ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->SetTranslationArray( data->GetArray() );
    }
}
////////////////////////////////////////////////////////////////////////////////
NavigationDataEventHandler& NavigationDataEventHandler::operator=( const NavigationDataEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
