/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iam_state.edu/~kmbryden
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

// --- My Includes --- //
#include "CustomKeyboardMouseInterface.h"

// --- vrJuggler Includes --- //
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CustomKeyboardMouseInterface::CustomKeyboardMouseInterface()
:
mMousePosition( 0, 0 )
{
    mKeyboard.init( "VJKeyboard" );
}
////////////////////////////////////////////////////////////////////////////////
CustomKeyboardMouseInterface::~CustomKeyboardMouseInterface()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CustomKeyboardMouseInterface::ProcessEvents()
{
    gadget::KeyboardMouse::EventQueue eventQueue = mKeyboard->getEventQueue();
    if( eventQueue.empty() )
    {
        return false;
    }

    bool leftMouseButtonState( false );
    gadget::KeyboardMouse::EventQueue::iterator i;
    for( i = eventQueue.begin(); i != eventQueue.end(); ++i )
    {
        const gadget::EventType type = ( *i )->type();

        if( type == gadget::MouseButtonPressEvent )
        {
            gadget::MouseEventPtr mouseEvent =
                boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );
            mMousePosition.first = mouseEvent->getX();
            mMousePosition.second = mouseEvent->getY();

            if( mouseEvent->getButton() == gadget::MBUTTON1 )
            {
                leftMouseButtonState = true;
            }
        }
    }

    return leftMouseButtonState;
}
////////////////////////////////////////////////////////////////////////////////
std::pair< unsigned int, unsigned int > CustomKeyboardMouseInterface::
    GetMousePosition()
{
    return mMousePosition;
}
////////////////////////////////////////////////////////////////////////////////
