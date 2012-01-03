/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CE_UTILITIES_NETWORK_H
#define CE_UTILITIES_NETWORK_H
#include <ves/VEConfig.h>
#include <ves/open/moduleS.h>
#include <ves/open/xml/model/NetworkPtr.h>
#include <ves/open/xml/model/SystemPtr.h>

#include <string>
#include <vector>
#include <map>

namespace VE_CE
{
namespace Utilities
{
class Module;
class Connection;

///??
class VE_CE_UTILS_EXPORTS Network
{
public:
    ///Constructor
    Network();
    ///Destructor
    ~Network();
    ///Basically is a destructor
    void clear( void );
    //Pass the string in from corba
    int parse( std::string xmlNetwork );
    ///Get the present network and pass it to vesuite
    std::string GetNetworkString( void );
    ///Return the number of modules
    int nmodules();

    ///The module of interest is passed in and the
    /// vector index is returned
    int GetModuleIndex( Module* );
    ///idx is the actual vector index
    ///\param idx The actual vector index for a module
    Module* GetModule( int idx );
    ///idx is the conductor assigned index. the vector index is returned
    ///\param idx The Conductor assigned idx
    int moduleIdx( int idx );

protected:
    ///add a new module to the network
    void add_module( unsigned int );
    ///The list of Connections
    std::vector< Connection* > _connections;
    ///The list of module
    std::vector< Module* > _module_ptrs;
    /// This map should be used in the future. This would allow
    /// easy access to modules from conductor ids.
    std::map< unsigned int, Module* > moduleIDMap;
    /// Holder of the network
    ves::open::xml::model::NetworkPtr veNetwork;
    ///Holds the top level system for the current network
    ves::open::xml::model::SystemPtr mSystemPtr;
};
}
}
#endif
