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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VE_VOICE_INTERPRETER_H
#define VE_VOICE_INTERPRETER_H

#include "VEMsgInterpreter.h"

// Base class for a voice interpreter,
// in case something other than SAPI gets used.

class /*VE_VOICE_EXPORTS*/ VEVoiceInterpreter : public VEMsgInterpreter
{
public:
    VEVoiceInterpreter( int argc, char** argv );
    virtual ~VEVoiceInterpreter()
    {
        ;
    }

    virtual void Listen() = 0;
    // Listen() Implementations should invoke ProcessPhrase to send commands.
    bool ProcessPhrase( std::string phrase );

protected:

    void SetStepMode();
    void SetGlideMode();
    void StopMoving();
    void Quit();
    void SendLevelCmd();
    void SendPointCmd();
    void SendPickCmd();
    void SendResetCmd();
    void SendReleaseCmd();
    void InterpretMoveMessage( std::string navMsg );
    void InterpretStepMessage( std::string navMsg );

    bool _glide;
    bool _step;
};

#endif // VE_VOICE_INTERPRETER_H
