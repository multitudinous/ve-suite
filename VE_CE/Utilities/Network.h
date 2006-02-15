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
#include <map>

namespace VE_Model
{
   class Network;
}

namespace VE_XML
{
   class Command;
}

namespace VE_CE
{
namespace Utilities
{
class Module;
class Connection;

class VE_CE_UTILS_EXPORTS Network 
{
public:
   ///Constructor
   Network();
   ///Destructor
   ~Network ();
   ///Basically is a destructor
   void clear( void );
   //Pass the string in from corba
   int parse( std::string xmlNetwork );
   ///Get the present network and pass it to vesuite
   std::string GetNetworkString( void );

   int nmodules();
   void add_module( int, std::string );
   
   ///The module of interest is passed in and the
   /// vector index is returned
   int GetModuleIndex( Module* );
   ///idx is the actual vector index
   Module* GetModule( int idx );
   ///idx is the conductor assigned index. the vector index is returned
   int moduleIdx( int idx );

/*
   int addIPort( int, int, Connection* );
   int addOPort( int, int, Connection* );

   int getInput(int, VE_XML::Command& );
   int setInput(int, VE_XML::Command* );

   int getOutput(int, VE_XML::Command& );
   int setOutput(int, VE_XML::Command* );

   int getMessage(int, VE_XML::Command& );
   int setMessage(int, VE_XML::Command* );

   int getPortData(int, int, VE_XML::Command& );
   int setPortData(int, int, VE_XML::Command* );

   int getPortProfile(int, int, Types::Profile_out& );
   int setPortProfile(int, int, const Types::Profile* );
*/
protected:
   std::vector<Connection*> _connections;
   std::vector<Module*> _module_ptrs;
   /// This map should be used in the future. This would allow
   /// easy access to modules from conductor ids.
   std::map< int, Module* > moduleIDMap;
   /// Holder of the network
   VE_Model::Network* veNetwork;
};
}
}
#endif
