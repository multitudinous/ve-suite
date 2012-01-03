/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

class Body_Unit_i;

// --- wxWidgets Includes --- //
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32

// --- C/C++ Includes --- //
#include <string>

class CorbaUnitManager
{
public:
    ///Constructor
    CorbaUnitManager();

    ///Destructor
    ~CorbaUnitManager();

    ///
    void SetRunORBFlag( bool run );

    ///
    bool RunORB(
        std::string& workingDirectory,
        std::string& computerName,
        std::string& portNumber );

    ///
    void CheckCORBAWork();

    ///
    void DestroyORB();

    ///
    bool CleanUp();

    ///
    Body_Unit_i* GetUnitObject();

protected:

private:
    ///
    bool unit_i_instantiated;

    ///
    Body_Unit_i* unit_i;

    ///
    CORBA::ORB_var orb;

    ///
    PortableServer::POA_var poa;

    ///
    Body::Executive_var exec;

};

#endif //CORBA_UNIT_MANAGER
