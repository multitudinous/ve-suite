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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef VE_MSG_INTERPRETER_H
#define VE_MSG_INTERPRETER_H

#include "VE_Installer/include/VEConfig.h"

#include <iostream>
#include <string>
#include <orbsvcs/CosNamingC.h>
#include "VE_Open/skel/moduleC.h"
#include "VE_Open/skel/VjObsC.h"

//we may want this to be a dll that gets loaded optionally later

class /* VE_VOICE_EXPORTS */ VEMsgInterpreter
{
public:
    VEMsgInterpreter( int argc, char** argv );
    VEMsgInterpreter( int argc, char** argv, VjObs_ptr server );
    virtual ~VEMsgInterpreter();

    bool IsRunning();
    void SetServer( VjObs_ptr _server );

protected:
    void _initCORBA( int argc, char** argv );
    void _shutdownCORBA();
    void _sendVoiceCommandToVE();

    short _numScalars;
    short _numVectors;
    short _numGeoArrays;
    int   _clients;
    int   _iso_value;
    int   _sc;
    int   _min;
    int   _max;
    long  _id;
    long  _geo_state;
    short _postdata_state;
    bool  _pre_state;
    short _timesteps;
    short _numTeacherArrays;
    short _teacher_state;
    int _numOfClientInfo;
    bool _quit;

    VjObs::obj_pd_var _clientInfoArray;
    VjObs_ptr _server;
};
#endif // VE_MSG_INTERPRETER_H
