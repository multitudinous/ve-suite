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

// --- VE_PowersimUnit Includes --- //
class Body_Unit_i;
class CMainDlg;
class SIPParser;

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

// --- ATL Includes --- //
#include <atlstr.h>

class CorbaUnitManager
{
public:
    ///
    CorbaUnitManager( CMainDlg* mainDialog );

    ///
    ~CorbaUnitManager();

    ///
    void SetComputerNameUnitNameAndPort(
        const ATL::CString& workingDir,
        const ATL::CString& computerName,
        const ATL::CString& computerPort,
        const ATL::CString& unitName );

    ///
    void SetRunORBFlag( bool run );

    ///
    void RunORB();

    ///
    void DestroyORB();

    ///
    Body_Unit_i* const GetUnitObject() const;

    ///
    void CheckCORBAWork();

    ///
    //SIPParser* CreateParser();

    ///
    bool unit_i_instantiated;

    ///
    bool CleanUp();

protected:

private:
    ATL::CString m_workingDir;
    ATL::CString m_computerName;
    ATL::CString m_computerPort;
    ATL::CString m_unitName;

    Body_Unit_i* unit_i;
    CORBA::ORB_var orb;
    PortableServer::POA_var poa;
    CMainDlg* parent;
    Body::Executive_var exec;

};

#endif //CORBA_UNIT_MANAGER
