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
#include "VEPSI_i.h"
#include "VE_PSI.h"
#include "VE_PSIDlg.h"
#include "AspenPlus.h"
#include "AspenDynamics.h"
#include "DynSim.h"
#include <vpr/Thread/Thread.h>

class CorbaUnitManager
{
public:
    CorbaUnitManager(VE_PSIDlg *);
   ~CorbaUnitManager();
   void SetComputerNameUnitNameAndPort( std::string dir, std::string name, std::string port, std::string uname );
   void SetRunORBFlag( bool run );
   bool RunORB( void );
   void DestroyORB( void );
   VEPSI_i* GetUnitObject( void );
   void CheckCORBAWork( void );
   AspenPlus * CreateParser( void );
   void CheckCORBAWorkThread(  );

   bool unit_i_instantiated;
   bool CleanUp( );
   Body::Executive_ptr GetExecutive(  );

private:
   std::string workingDir;
   std::string computerName;
   std::string computerPort;
   std::string unitName;
   VEPSI_i* unit_i;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   VE_PSIDlg * parent;
   Body::Executive_var exec;
   vpr::Thread* m_thread;
   bool m_running;
};
#endif
