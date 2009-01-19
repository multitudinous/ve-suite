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
#ifndef SAPI_VOICE_INTERPRETER_H
#define SAPI_VOICE_INTERPRETER_H
#ifdef WIN32

// Prevent winsock 1 from getting loaded (sapi.h -> windows.h -> winsock.h).
// Something somewhere else uses winsock 2, and the two don't get along.
#define _WINSOCKAPI_
#include <sapi.h>
#include <sphelper.h>
#include "VEVoiceInterpreter.h"

class /* VE_VOICE_EXPORTS */ SAPIVoiceInterpreter : public VEVoiceInterpreter
{
public:
    SAPIVoiceInterpreter( int argc, char** argv );
    ~SAPIVoiceInterpreter();
    void Listen();
    bool SAPIInit();
protected:
    CComPtr<ISpRecognizer>  rec;
    CComPtr<ISpRecoGrammar> grammar;
    CComPtr<ISpRecoContext> context;
    CComPtr<ISpAudio>       audio;
    std::string cmdsFile;
    int ConfigureSAPI( std::string grammar_file );
    // SAPI can't do non-static member callbacks.
    // http://support.microsoft.com/kb/q102352/ is relevant.
    // The this pointer of a SAPIVoiceInterpreter instance can be passed in
    // the LPARAM via reinterpret_cast'ing, though, which is done here.
    // This works on Win32, and it should work on Win64 since LPARAM becomes 8 bytes.
    static void __stdcall SAPICallback( WPARAM wp, LPARAM lp );

};

#endif // WIN32
#endif // SAPI_VOICE_INTERPRETER_H
