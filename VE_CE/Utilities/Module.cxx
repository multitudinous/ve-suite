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
#include "VE_CE/Utilities/Module.h"
#include "VE_CE/Utilities/OPort.h"
#include "VE_CE/Utilities/IPort.h"
#include "VE_CE/Utilities/Network.h"
#include "VE_CE/Utilities/Connection.h"

using namespace VE_CE::Utilities;

////////////////////////////////////////////////////////////////////////////////
Module::Module (int id, Network* net)
  : _need_execute (true),
    _return_state (0),
    _is_feedback  (0),
    _type         (-1),
    _category     (-1),
    _net          (net),
    _id           (id)
{
}
////////////////////////////////////////////////////////////////////////////////
Module::Module (const Module &m)
{
  copy(m);
}
////////////////////////////////////////////////////////////////////////////////
Module::~Module ()
{
   for ( size_t i=0; i < _iports.size(); ++i ) 
   {
      delete _iports[i];
   }
   _iports.clear();
   
   for ( size_t i=0; i < _oports.size(); ++i ) 
   {
      delete _oports[i];
   }
   _oports.clear();
   
}
////////////////////////////////////////////////////////////////////////////////
void Module::copy (const Module &m)
{
  if(this==&m) return;

  _need_execute = m._need_execute;
  _inputs       = m._inputs;
  _outputs      = m._outputs;
  _iports       = m._iports;
  _oports       = m._oports;
  _net          = m._net;
  _id           = m._id;
  _type         = m._type;
  _category     = m._category;
}
////////////////////////////////////////////////////////////////////////////////
int Module::numOPorts ()
{
  return (int)_oports.size();
}
////////////////////////////////////////////////////////////////////////////////
int Module::numIPorts ()
{
  return (int)_iports.size();
}
////////////////////////////////////////////////////////////////////////////////
OPort* Module::getOPort( int idx )
{
   try
   {
      return _oports.at( idx );
   }
   catch ( ... )
   {
      return NULL;
   }
}
////////////////////////////////////////////////////////////////////////////////
IPort* Module::getIPort( int idx )
{
   try
   {
      return _iports.at( idx );
   }
   catch ( ... )
   {
      return NULL;
   }
}
////////////////////////////////////////////////////////////////////////////////
IPort* Module::getFBPort( void )
{
   for( size_t i=0; i < _iports.size(); ++i )
   {
     if ( _iports[i]->get_id() == 1 ) 
         return _iports[ i ];
   }

   return NULL;
}
////////////////////////////////////////////////////////////////////////////////
int Module::get_id ()
{
  return _id;
}
////////////////////////////////////////////////////////////////////////////////
int Module::iportIdx( int idx )
{
   ///Get the vector index for the specific port id 
   for( size_t i = 0; i < _iports.size(); ++i )
   {
      if ( _iports[i]->get_id() == idx ) 
      {
         return i;
      }
   }

   return -1;
}
////////////////////////////////////////////////////////////////////////////////
int Module::oportIdx( int idx )
{
   ///Get the vector index for the specific port id 
   for ( size_t i=0; i < _oports.size(); ++i )
   {
      if ( _oports[i]->get_id() == idx ) 
      {
         return i;
      }
   }

   return -1;
}
////////////////////////////////////////////////////////////////////////////////
void Module::addIPort( int p, Connection* c )
{
   size_t sz = _iports.size();
   int fi = iportIdx( p );
  
   if ( fi < 0 ) 
   {
      fi = sz;
      _iports.push_back( new IPort(p, this) );
   }

   _iports[ fi ]->add_connection( c );
   c->connect_iport( _iports[fi] );
}
////////////////////////////////////////////////////////////////////////////////
void Module::addOPort( int p, Connection* c )
{
   size_t sz = _oports.size();
   int fi = oportIdx( p );

   if ( fi < 0 ) 
   {
      fi = sz;
      _oports.push_back( new OPort(p, this) );
   }

   _oports[fi]->add_connection(c);
   c->connect_oport(_oports[fi]);
}
////////////////////////////////////////////////////////////////////////////////
int Module::getPortData( int p, Interface& intf )
{
   int fi = oportIdx( p );
   if ( fi < 0 ) 
      return 0;

   intf.copy( _oports[fi]->_data );
   return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Module::setPortData( int p, Interface* intf )
{
   int fi = oportIdx(p);
   
   if ( fi < 0 ) 
      return 0;
   
   _oports[fi]->_data.copy(*intf);
   return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Module::getPortProfile( int p, Types::Profile_out& prof )
{
   int fi = oportIdx( p );  
   if ( fi < 0 ) 
      return 0; 
   
   prof = new Types::Profile( *(_oports[fi]->_profile) );
   return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Module::setPortProfile (int p, const Types::Profile* prof)
{
   int fi = oportIdx(p);
   if ( fi < 0 ) 
      return 0;

   if ( _oports[fi]->_profile ) 
   {
      delete _oports[fi]->_profile;
   }

   _oports[fi]->_profile = new Types::Profile(*prof); 
   return 1;
}
