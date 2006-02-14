/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: Network_Exec.h,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CE_UTILITIES_NETWORK_H
#define CE_UTILITIES_NETWORK_H
#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Framework/interface.h"
#include "VE_Open/skel/moduleS.h"
#include <string>
#include <vector>

namespace VE_CE
{
namespace Utilities
{
class Module;
class Connection;

class VE_CE_UTILS_EXPORTS Network 
{
public:
   Network  ();
   ~Network ();

   void clear ();
   int  parse (Interface *);

   int nmodules   ();
   void         add_module (int, std::string);
   int GetModuleIndex( Module* );
   Module* GetModule( int );
   int moduleIdx  (int);

   int addIPort (int, int, Connection*);
   int addOPort (int, int, Connection*);

   int getInput (int, Interface&);
   int setInput (int, Interface*);

   int getGeomInput(int, Interface&);
   int setGeomInput(int, Interface*);

   int getOutput (int, Interface&);
   int setOutput (int, Interface*);

   int getMessage (int, Interface&);
   int setMessage (int, Interface*);

   int getPortData (int, int, Interface&);
   int setPortData (int, int, Interface*);

   int getPortProfile (int, int, Types::Profile_out&);
   int setPortProfile (int, int, const Types::Profile*);

   std::vector<Connection*> _connections;

protected:
  std::vector<Module*> _module_ptrs;

};
}
}
#endif
