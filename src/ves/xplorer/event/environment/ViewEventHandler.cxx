/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

// --- VE-Suite Includes --- //
#include <ves/xplorer/event/environment/ViewEventHandler.h>
#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- VR Juggler Includes --- //
#include <boost/filesystem/operations.hpp> //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

// --- C/C++ Libraries --- //
#include <string>

using namespace ves::xplorer::event;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
ViewEventHandler::ViewEventHandler()
        :
        ves::xplorer::event::EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ViewEventHandler::ViewEventHandler( const ViewEventHandler& rhs )
        :
        ves::xplorer::event::EventHandler( rhs )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ViewEventHandler::~ViewEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ViewEventHandler::SetGlobalBaseObject( ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ViewEventHandler::Execute(
    const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    ves::open::xml::CommandPtr command =
        boost::dynamic_pointer_cast< ves::open::xml::Command >( veXMLObject );

    ves::open::xml::DataValuePairPtr viewDVP = command->GetDataValuePair( "View" );
    if( !viewDVP )
    {
        return;
    }

    std::string view;
    viewDVP->GetData( view );
    device::KeyboardMouse* keyboardMouse =
        DeviceHandler::instance()->GetDevice(
            device::Device::KEYBOARD_MOUSE )->AsKeyboardMouse();
    if( view == "Frame All" )
    {
        keyboardMouse->FrameAll();
    }
    else if( view == "Frame Selection" )
    {
        keyboardMouse->FrameSelection();
    }
    else if( view == "Reset" )
    {
        keyboardMouse->ResetTransforms();
    }
}
////////////////////////////////////////////////////////////////////////////////
ViewEventHandler& ViewEventHandler::operator=( const ViewEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
