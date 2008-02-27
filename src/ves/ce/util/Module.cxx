/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/ce/util/Module.h>
#include <ves/ce/util/OPort.h>
#include <ves/ce/util/IPort.h>
#include <ves/ce/util/Connection.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Port.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace VE_CE::Utilities;
using namespace ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
Module::Module()
:
    _need_execute( true ),
    _return_state( 0 ),
    _is_feedback( 0 )
{
   veModel = new model::Model();
}
////////////////////////////////////////////////////////////////////////////////
Module::Module( const Module &m )
{
   veModel = new model::Model();
   copy( m );
}
////////////////////////////////////////////////////////////////////////////////
Module::~Module()
{
   for( size_t i = 0; i < _iports.size(); ++i )
   {
       delete _iports[i];
   }
   _iports.clear();

   for( size_t i = 0; i < _oports.size(); ++i )
   {
       delete _oports[i];
   }
   _oports.clear();

   inputs.clear();
   results.clear();
   ports.clear();
}
////////////////////////////////////////////////////////////////////////////////
void Module::copy( const Module &m )
{
   if( this == &m ) return;

   _need_execute = m._need_execute;
   _iports       = m._iports;
   _oports       = m._oports;
   _id           = m._id;
   *veModel = *( m.veModel );

   inputs.clear();
   inputs = m.inputs;

   results.clear();
   results = m.results;

   ports.clear();
   for( size_t i = 0; i < m.ports.size(); ++i )
   {
       ports.push_back( new model::Port( *( m.ports.at( i ) ) ) );
   }
}
////////////////////////////////////////////////////////////////////////////////
size_t Module::numOPorts()
{
   return _oports.size();
}
////////////////////////////////////////////////////////////////////////////////
size_t Module::numIPorts()
{
   return _iports.size();
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
   for( size_t i = 0; i < _iports.size(); ++i )
   {
       if( _iports[i]->get_id() == 1 )
           return _iports[ i ];
   }

   return NULL;
}
////////////////////////////////////////////////////////////////////////////////
int Module::get_id()
{
   return _id;
}
////////////////////////////////////////////////////////////////////////////////
int Module::iportIdx( int idx )
{
   ///Get the vector index for the specific port id
   for( size_t i = 0; i < _iports.size(); ++i )
   {
       if( _iports[i]->get_id() == idx )
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
   for( size_t i = 0; i < _oports.size(); ++i )
   {
       if( _oports[i]->get_id() == idx )
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

   if( fi < 0 )
   {
       fi = sz;
       _iports.push_back( new IPort( p, this ) );
   }

   _iports[ fi ]->add_connection( c );
   c->connect_iport( _iports[fi] );
}
////////////////////////////////////////////////////////////////////////////////
void Module::addOPort( int p, Connection* c )
{
   size_t sz = _oports.size();
   int fi = oportIdx( p );

   if( fi < 0 )
   {
       fi = sz;
       _oports.push_back( new OPort( p, this ) );
   }

   _oports[fi]->add_connection( c );
   c->connect_oport( _oports[fi] );
}
////////////////////////////////////////////////////////////////////////////////
int Module::getPortData( int p, CommandPtr intf )
{
   try
   {
       intf = ( _oports.at( oportIdx( p ) )->GetPortData() );
       return 1;
   }
   catch ( ... )
   {
       return 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
int Module::setPortData( int p, CommandPtr intf )
{
   try
   {
       _oports.at( oportIdx( p ) )->SetPortData( intf );
       return 1;
   }
   catch ( ... )
   {
       return 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
int Module::getPortProfile( int p, Types::Profile_out& prof )
{
   int fi = oportIdx( p );
   if( fi < 0 )
       return 0;

   prof = new Types::Profile( *( _oports[fi]->_profile ) );
   return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Module::setPortProfile( int p, const Types::Profile* prof )
{
   int fi = oportIdx( p );
   if( fi < 0 )
       return 0;

   if( _oports[fi]->_profile )
   {
       delete _oports[fi]->_profile;
   }

   _oports[fi]->_profile = new Types::Profile( *prof );
   return 1;
}
////////////////////////////////////////////////////////////////////////////////
std::string Module::GetModuleName( void )
{
   return _name;
}
////////////////////////////////////////////////////////////////////////////////
model::ModelWeakPtr Module::GetVEModel( void )
{
   //Set the input, results, port data data structures
   return veModel;
}
////////////////////////////////////////////////////////////////////////////////
void Module::SetVEModel( model::ModelWeakPtr mod )
{
   veModel = mod;
   //Set the name of this module
   _name = veModel->GetModelName();
   // _id is set in the constructor
   _id = veModel->GetModelID();
   _need_execute = 1;
   _return_state = 0;

   ///Get feedback info
   for( size_t i = 0; i < veModel->GetNumberOfInputs(); ++i )
   {
       DataValuePairPtr dvp = veModel->GetInput( i )->GetDataValuePair( "FEEDBACK" );
       if( dvp )
       {
           unsigned int feedback;
           dvp->GetData( feedback );
           _is_feedback = static_cast< int >( feedback );
           break;
       }
   }

   //Now get port data
   ports.clear();
   for( size_t i = 0; i < veModel->GetNumberOfPorts(); ++i )
   {
       ports.push_back( veModel->GetPort( i ) );
   }

   //Probably now need to set port data pointers on the port vectors
}
////////////////////////////////////////////////////////////////////////////////
std::vector< CommandPtr > Module::GetInputData( void )
{
   inputs.clear();
   for( size_t i = 0; i < veModel->GetNumberOfInputs(); ++i )
   {
       inputs.push_back( veModel->GetInput( i ) );
   }
   return inputs;
}
////////////////////////////////////////////////////////////////////////////////
void Module::SetInputData( std::vector< XMLObjectPtr > inputData )
{
   for( size_t i = 0; i < inputData.size(); ++i )
   {
       CommandPtr tempCommand = inputData.at( i );
       veModel->SetInput( tempCommand );
   }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< CommandPtr > Module::GetResultsData( void )
{
   results.clear();
   for( size_t i = 0; i < veModel->GetNumberOfResults(); ++i )
   {
       results.push_back( veModel->GetResult( i ) );
   }
   return results;
}
////////////////////////////////////////////////////////////////////////////////
void Module::SetResultsData( std::vector< XMLObjectPtr > resultsData )
{
   for( size_t i = 0; i < resultsData.size(); ++i )
   {
       CommandPtr tempCommand = resultsData.at( i );
       veModel->SetResult( tempCommand );
   }
}
