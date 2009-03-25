/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/

#include <ves/xplorer/event/device/KeyboardMouseEH.h>

#include <ves/xplorer/GlobalBase.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace ves::xplorer::event;

////////////////////////////////////////////////////////////////////////////////
KeyboardMouseEventHandler::KeyboardMouseEventHandler()
    :
    EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouseEventHandler::KeyboardMouseEventHandler(
    const KeyboardMouseEventHandler& rhs )
    :
    EventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouseEventHandler::~KeyboardMouseEventHandler()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouseEventHandler::SetGlobalBaseObject(
    ves::xplorer::GlobalBase* modelHandler )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouseEventHandler::Execute(
    const ves::open::xml::XMLObjectPtr& veXMLObject )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouseEventHandler& KeyboardMouseEventHandler::operator=(
    const KeyboardMouseEventHandler& rhs )
{
    if( this != &rhs )
    {
        ves::xplorer::event::EventHandler::operator=( rhs );
    }

    return *this;
}
////////////////////////////////////////////////////////////////////////////////
