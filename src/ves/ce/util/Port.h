/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CE_UTILITIES_PORT_H
#define CE_UTILITIES_PORT_H
/*!\file VE_CE/Utilities/Port.h
 * VE-CE Port API
 */
#include <VE_Installer/include/VEConfig.h>
#include <vector>

namespace VE_CE
{
namespace Utilities
{
class Connection;
class Module;

///Port base class
class VE_CE_UTILS_EXPORTS Port 
{
public:
   ///??
   Port( int, Module* );
   ///??
   Port( const Port& );
   virtual ~Port();

   ///??
   void copy (const Port&);

   ///??
   int         nconnections   ();
   ///??
   Connection* connection     (int);
   ///??
   void        add_connection (Connection*);

   ///??
   Module* get_module( void );
   ///Get the port id
   int get_id( void );

protected:
   ///??
   std::vector<Connection*> _connections;
   /// Module this port is connected to
   Module* _module;
   /// id of the port
   int _id;
};
}
}
#endif
