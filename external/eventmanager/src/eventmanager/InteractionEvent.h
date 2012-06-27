/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#pragma once

#include <eventmanager/Exports.h>


namespace eventmanager
{
/// @file InteractionEvent.h
/// @namespace eventmanager
/** @class InteractionEvent
  * A class that can be used to express keyboard and mouse events compactly.
  * The eventmanager library does use this internally, but we provide it here
  * for application developers since custom handling of mouse and keyboard
  * events is a common application requirement. This class is intended to be
  * used as the argument to a signal that passes around keyboard and mouse
  * events. Using this class along with a BooleanPropagationCombiner will allow
  * you to easily mimick the common practice of sinking keyboard and mouse
  * events in certain classes.
  **/
class EVENTMANAGER_EXPORT InteractionEvent
{
public:

    enum eventType
    {
        keyPress, keyRelease, buttonPress, buttonRelease, pointerMotion, scroll
    };

    enum buttonType
    {
        button_none = 0x0000, button_1 = 0x0001, button_2 = 0x0002, button_3 = 0x0004,
        button_4 = 0x0008, button_5 = 0x0010, button_6 = 0x0020,
        button_7 = 0x0040, button_8 = 0x0080, button_9 = 0x0100,
        button_10 = 0x0200, button_11 = 0x0400, button_12 = 0x0800,
        button_13 = 0x1000, button_14 = 0x2000, button_15 = 0x4000,
        button_16 = 0x8000
    };

    enum modifierType
    {
        NONE_MASK    = 0,
        SHIFT_MASK   = (1 << 0),
        ALT_MASK     = (1 << 1),
        CTRL_MASK    = (1 << 2),
        COMMAND_MASK = (1 << 3)
    };

    InteractionEvent( eventType eType,
                      int key,
                      const char keyChar,
                      const wchar_t keyUnicode,
                      int modifiers,
                      buttonType button = button_none,
                      int buttons = button_none,
                      float scrollDeltaX = 0.0,
                      float scrollDeltaZ = 0.0,
                      double x = 0.0,
                      double y = 0.0,
                      double z = 0.0 );

    virtual ~InteractionEvent();

    eventType EventType;
    int Key;
    char KeyChar;
    wchar_t KeyUnicode;
    int Modifiers;
    buttonType Button;
    int Buttons;
    float ScrollDeltaX;
    float ScrollDeltaZ;
    double X;
    double Y;
    double Z;


private:

};
} //end eventmanager

