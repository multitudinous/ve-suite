/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/skel/VjObsS.h"
#include <orbsvcs/CosNamingC.h>

#include "VEVoiceInterpreter.h"
#include "VE_Xplorer/cfdDebug.h"
#include "VE_Xplorer/cfdEnum.h"

VEVoiceInterpreter::VEVoiceInterpreter( int argc, char** argv ) : VEMsgInterpreter( argc, argv )
{
    ;
}

////////////////////////////////////////
void VEVoiceInterpreter::Quit()
{
    _quit = true;
    _id = EXIT;
    _sendVoiceCommandToVE();
}
////////////////////////////////////////////////
void VEVoiceInterpreter::SetStepMode()
{
    _glide = false;
    _step = true;
}
////////////////////////////////////////////////
void VEVoiceInterpreter::SetGlideMode()
{
    _glide = true;
    _step = false;
}
/////////////////////////////////////////////////////////////////////
void VEVoiceInterpreter::StopMoving()
{
    _id = -1;
    _iso_value = -1;
    _sendVoiceCommandToVE();
}
////////////////////////////////////////////////
void VEVoiceInterpreter::SendLevelCmd()
{
    _id = GOTO_Z;
    _iso_value = 0;
    _sendVoiceCommandToVE();
}
////////////////////////////////////////////////
void VEVoiceInterpreter::SendPointCmd()
{
    _id = GEOMETRY_PICKING;
    _sc = 2;
    _sendVoiceCommandToVE();
}
////////////////////////////////////////////////
void VEVoiceInterpreter::SendPickCmd()
{
    _id = SELECT_GEOMETRY;
    _iso_value = 1;
    _sendVoiceCommandToVE();
}
////////////////////////////////////////////////
void VEVoiceInterpreter::SendResetCmd()
{
    _id = RESET_PICKED_GEOMETRY;
    _sendVoiceCommandToVE();
}
////////////////////////////////////////////////
void VEVoiceInterpreter::SendReleaseCmd()
{
    _id = SELECT_GEOMETRY;
    _iso_value = 0;
    _sendVoiceCommandToVE();
}
////////////////////////////////////////////////////////////////////////////
void VEVoiceInterpreter::InterpretMoveMessage( std::string voiceCmd )
{
    //tell cfdApp to move appropriately
    _id = GUI_NAV;
    if( !strcmp( "Move Up", voiceCmd.c_str() ) )
    {
        _iso_value = NAV_UP;
    }
    else if( !strcmp( "Move Down", voiceCmd.c_str() ) )
    {
        _iso_value = NAV_DOWN;
    }
    else if( !strcmp( "Move Left", voiceCmd.c_str() ) )
    {
        _iso_value = NAV_LEFT;
    }
    else if( !strcmp( "Move Right", voiceCmd.c_str() ) )
    {
        _iso_value = NAV_RIGHT;
    }
    else if( !strcmp( "Move In", voiceCmd.c_str() ) )
    {
        _iso_value = NAV_FWD;
    }
    else if( !strcmp( "Move Out", voiceCmd.c_str() ) )
    {
        _iso_value = NAV_BKWD;
    }

    _sendVoiceCommandToVE();

    //tell cfdNavigate to stop by resetting the cmdArray
    if( _step )
    {
        StopMoving();
    }
}
/////////////////////////////////////////////////////////
bool VEVoiceInterpreter::ProcessPhrase( std::string phrase )
{
    if( strstr( phrase.c_str(), "Move" ) ) // Most of the nav commands have MOVE in them
    {
        this->InterpretMoveMessage( phrase );
    }
    else if( strstr( phrase.c_str(), "Stop that" ) )
    {
        this->StopMoving();
    }
    else if( strstr( phrase.c_str(), "Step" ) )
    {
        this->SetStepMode();
    }
    else if( strstr( phrase.c_str(), "Glide" ) )
    {
        this->SetGlideMode();
    }
    else if( !strcmp( phrase.c_str(), "Level" ) )
    {
        this->SendLevelCmd();
    }
    else if( strstr( phrase.c_str(), "Point" ) ) // Match "Point" or "Pointer"
    {
        this->SendPointCmd();
    }
    else if( !strcmp( phrase.c_str(), "Pick" ) || !strcmp( phrase.c_str(), "Grab" ) )
    {
        this->SendPickCmd();
    }
    else if( !strcmp( phrase.c_str(), "Reset" ) )
    {
        this->SendResetCmd();
    }
    else if( !strcmp( phrase.c_str(), "Release" ) )
    {
        this->SendReleaseCmd();
    }
    else if( strstr( phrase.c_str(), "Quit" ) || strstr( phrase.c_str(), "Exit" ) || strstr( phrase.c_str(), "Goodbye" ) )
    {
        this->Quit();
    }
    else
    {
        vprDEBUG( vesDBG, 2 ) << "Interpreter doesn't know what to do with: "
        << phrase << "\n" << vprDEBUG_FLUSH;
        return false;
    }
    return true;

}
