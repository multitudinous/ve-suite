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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CE_UTILITIES_CONNECTION_H
#define CE_UTILITIES_CONNECTION_H
/* \file Connection.h
 * VE_CE Connection API
 */
#include "VE_Installer/include/VEConfig.h"

namespace VE_CE
{
namespace Utilities
{
class IPort;
class OPort;

///This class is basically a link
///It has an id, input port, output port
class VE_CE_UTILS_EXPORTS Connection 
{
public:
   ///??
   Connection(int);
   ///??
   Connection(const Connection&);
   ~Connection();

   ///??
   void copy (const Connection&);

   ///??
   IPort* get_iport ();
   ///??
   OPort* get_oport ();

   ///??
   void connect_iport (IPort*);
   ///??
   void connect_oport (OPort*);

   ///??
   int get_id();

private:
   IPort* _iport;
   OPort* _oport;

   int _id;
};
}
}
#endif
