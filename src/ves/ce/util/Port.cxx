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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_CE/Utilities/Port.h"
#include "VE_CE/Utilities/Connection.h"
#include "VE_CE/Utilities/Module.h"
using namespace VE_CE::Utilities;
////////////////////////////////////////////////////////////////////////////////
Port::Port (int id, Module* m)
  : _module (m),
    _id     (id)
{
}
////////////////////////////////////////////////////////////////////////////////
Port::Port (const Port& p)
{
  copy(p);
}
////////////////////////////////////////////////////////////////////////////////
Port::~Port ()
{
   /*for ( size_t i = 0; i < _connections.size(); ++i )
   {
      delete _connections.at( i );
   }
   _connections.clear();*/
/*
   if ( _module )
   {
      delete _module;
      _module = 0;
   }*/
}
////////////////////////////////////////////////////////////////////////////////
void Port::copy (const Port& p)
{
  if(this==&p) return;

  _connections = p._connections;
  _module      = p._module;
  _id          = p._id;
}
////////////////////////////////////////////////////////////////////////////////
int Port::nconnections ()
{
  return _connections.size();
}
////////////////////////////////////////////////////////////////////////////////
Connection* Port::connection(int idx)
{
   try
   {
      return _connections.at( idx );
   }
   catch ( ... )
   {
      return NULL;
   }
}
////////////////////////////////////////////////////////////////////////////////
void Port::add_connection( Connection* c )
{
   _connections.push_back( c );
}
////////////////////////////////////////////////////////////////////////////////
Module* Port::get_module()
{
  return _module;
}
////////////////////////////////////////////////////////////////////////////////
int Port::get_id()
{
  return _id;
}

