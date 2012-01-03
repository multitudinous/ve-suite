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
#include <ves/open/moduleS.h>
#include <string>
#include "AspenUnit_i.h"
#include "DynSimUnit.h"
#include "DynSimUnitDlg.h"
#include <vpr/Thread/Thread.h>

class CorbaUnitManager
{
public:
    CorbaUnitManager(CDynSimUnitDlg *);
   ~CorbaUnitManager();
   void SetComputerNameUnitNameAndPort( CString dir, CString name, CString port, CString uname );
   void SetRunORBFlag( bool run );
   void RunORB();
   void DestroyORB(  );
   AspenUnit_i* GetUnitObject(  );
   void CheckCORBAWorkThread(  );
   void CheckCORBAWork(  );
   //BKPParser * CreateParser( void );

   bool unit_i_instantiated;
   bool CleanUp( );
   Body::Executive_ptr GetExecutive(  );

private:
   CString workingDir;
   CString computerName;
   CString computerPort;
   CString unitName;
   AspenUnit_i* unit_i;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   CDynSimUnitDlg * parent;
   Body::Executive_var exec;
    vpr::Thread* m_thread;
};
#endif
