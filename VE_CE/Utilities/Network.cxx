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
#include "VE_CE/Utilities/Network.h"
#include "VE_CE/Utilities/Module.h"
#include "VE_CE/Utilities/Connection.h"

#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/Port.h"
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Network.h"

#include <set>
#include <iostream>

using namespace VE_CE::Utilities;
Network::Network ()
{
}
////////////////////////////////////////////////////////////////////////////////
Network::~Network ()
{
  clear();
}
////////////////////////////////////////////////////////////////////////////////
void Network::clear()
{
   for ( size_t i = 0; i < _module_ptrs.size(); ++i ) 
   {
      delete _module_ptrs[i];
   }
   _module_ptrs.clear();

   for ( size_t i = 0; i < _connections.size(); ++i ) 
   {
      delete _connections[i];
   }
   _connections.clear();
}
////////////////////////////////////////////////////////////////////////////////
int Network::parse( std::string xmlNetwork )
{
   // NOTE:This function assumes that the network as been cleared first
   // Load from the nt file loaded through wx
   // Get a list of all the command elements   
   VE_XML::XMLReaderWriter networkWriter;
   networkWriter.UseStandaloneDOMDocumentManager();
   networkWriter.ReadFromString();

   // do this for models
   networkWriter.ReadXMLData( xmlNetwork, "Model", "veModel" );
   std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

   // now lets create a list of them
   for ( size_t i = 0; i < objectVector.size(); ++i )
   {
      VE_Model::Model* model = dynamic_cast< VE_Model::Model* >( objectVector.at( i ) );
      add_module( model->GetModelID(), model->GetModelName() );
      GetModule( i )->SetVEModel( model );
   }

   //read for the network info now
   //this code must be second as the connections stuff assumes that the modules have already
   // been created
   networkWriter.ReadXMLData( xmlNetwork, "Model", "veNetwork" );
   objectVector = networkWriter.GetLoadedXMLObjects();

   // do this for network
   //if ( veNetwork )
   //   delete veNetwork;
   VE_Model::Network* veNetwork = 0;
   // we are expecting that a network will be found
   if ( !objectVector.empty() )
   {
      veNetwork = dynamic_cast< VE_Model::Network* >( objectVector.at( 0 ) );
   }
   else
   {
     std::cerr << "Improperly formated ves file." << "VES File Read Error" << std::endl;
     return 0;
   }

   /*
   veNetwork->GetDataValuePair( 0 )->GetData( (userScale.first)  );
   veNetwork->GetDataValuePair( 1 )->GetData( (userScale.second) );
   veNetwork->GetDataValuePair( 2 )->GetData( (numPix.first) );
   veNetwork->GetDataValuePair( 3 )->GetData( (numPix.second) );
   veNetwork->GetDataValuePair( 4 )->GetData( (numUnit.first) );
   veNetwork->GetDataValuePair( 5 )->GetData( (numUnit.second) );
   */

   for ( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++ i ) 
   {

      /*if ( FrMod.find(*iter)==FrMod.end()  || ToMod.find(*iter)==FrMod.end()   ||
           FrPort.find(*iter)==FrMod.end() || ToPort.find(*iter)==FrMod.end()) 
      {
         std::cerr << "Bad link found\n";
         return  0;
      }*/

      Connection* cn = new Connection( i );
      _connections.push_back( cn );

      long fromModuleID, toModuleID;
      veNetwork->GetLink( i )->GetFromModule()->GetData( fromModuleID );
      veNetwork->GetLink( i )->GetToModule()->GetData( toModuleID );

      long int toPort = *(veNetwork->GetLink( i )->GetToPort());
      long int fromPort = *(veNetwork->GetLink( i )->GetFromPort());

      if ( 
            !addIPort( toModuleID, toPort, cn ) ||
            !addOPort( fromModuleID, fromPort, cn )
         ) 
      {
         std::cerr << "Error adding ports" << std::endl;
         return 0;
      }
   }
   return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Network::addIPort( int m, int p, Connection* c )
{
   try
   {
      _module_ptrs.at( moduleIdx(m) )->addIPort(p, c);
      return 1;
   }
   catch ( ... )
   {
      return 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
int Network::addOPort (int m, int p, Connection* c)
{
   try
   {
      _module_ptrs.at( moduleIdx(m) )->addOPort(p, c);
      return 1;
   }
   catch ( ... )
   {
      return 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
int Network::nmodules ()
{
  return _module_ptrs.size();
}
////////////////////////////////////////////////////////////////////////////////
int Network::GetModuleIndex( Module* mod )
{
   for( size_t i = 0; i < _module_ptrs.size(); ++i )
   {
      if ( mod->get_id() == _module_ptrs[i]->get_id() ) 
         return i;
   }
   return -1;
}
////////////////////////////////////////////////////////////////////////////////
Module* Network::GetModule( int idx )
{
   try
   {
      return _module_ptrs.at( idx );
   }
   catch ( ... )
   {
      return NULL;   
   }
}
////////////////////////////////////////////////////////////////////////////////
int Network::moduleIdx( int id )
{
   ///This function goes from the veconductor assigned id
   ///to the vector id.
   for ( size_t i = 0; i < _module_ptrs.size(); ++i )
   {
      if ( id == _module_ptrs[ i ]->get_id() ) 
         return i;
   }
   return -1;
}
////////////////////////////////////////////////////////////////////////////////
void Network::add_module( int m, std::string name )
{
   if( moduleIdx(m) >= 0 ) 
      return;
  
   Module *mod = new Module( m );
   //mod->_name = name;
   _module_ptrs.push_back( mod );
}  
////////////////////////////////////////////////////////////////////////////////
int Network::getInput( int m, Interface& intf )
{
   try
   {
      intf.copy( _module_ptrs.at( moduleIdx( m ) )->_inputs );
      return 1;
   }
   catch ( ... )
   {
      return 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
int Network::setInput( int m, Interface* intf)
{
   try
   {
      _module_ptrs.at( moduleIdx(m) )->_inputs.copy(*intf);
      return 1;
   }
   catch (...)
   {
      return 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
int Network::getOutput (int m, Interface &intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_outputs);
  return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Network::setOutput (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_outputs.copy(*intf);
  return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Network::getMessage (int m, Interface &intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_messages);
  return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Network::setMessage (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_messages.copy(*intf);
  return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Network::getPortData (int m, int p, Interface& intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->getPortData(p, intf);
}
////////////////////////////////////////////////////////////////////////////////
int Network::setPortData (int m, int p, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->setPortData(p, intf);
}
////////////////////////////////////////////////////////////////////////////////
int Network::getPortProfile (int m, int p, Types::Profile_out& prof)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->getPortProfile(p, prof);
}
////////////////////////////////////////////////////////////////////////////////
int Network::setPortProfile (int m, int p, const Types::Profile* prof)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->setPortProfile(p, prof);
}
////////////////////////////////////////////////////////////////////////////////
std::string Network::GetNetworkString( void )
{/*
std::string Network::Save( std::string fileName )
{
   // Here we wshould loop over all of the following
   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   //  Newtork
   if ( veNetwork )
      delete veNetwork;
   
   veNetwork = new VE_Model::Network();
   nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( veNetwork, "veNetwork" ) );

   veNetwork->GetDataValuePair( -1 )->SetData( "m_xUserScale", userScale.first );
   veNetwork->GetDataValuePair( -1 )->SetData( "m_yUserScale", userScale.second );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixX", static_cast< long int >( numPix.first ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nPixY", static_cast< long int >( numPix.second ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitX", static_cast< long int >( numUnit.first ) );
   veNetwork->GetDataValuePair( -1 )->SetData( "nUnitY", static_cast< long int >( numUnit.second ) );

   for ( size_t i = 0; i < links.size(); ++i )
   {
      VE_Model::Link* xmlLink = veNetwork->GetLink( -1 );
      //xmlLink->GetFromPort()->SetData( modules[ links[i].GetFromModule() ].GetPlugin()->GetModelName(), links[i].GetFromPort() );
      //xmlLink->GetToPort()->SetData( modules[ links[i].GetToModule() ].pl_mod->GetModelName(), links[i].GetToPort() );
      xmlLink->GetFromModule()->SetData( modules[ links[i].GetFromModule() ].GetClassName(), static_cast< long int >( links[i].GetFromModule() ) );
      xmlLink->GetToModule()->SetData( modules[ links[i].GetToModule() ].GetClassName(), static_cast< long int >( links[i].GetToModule() ) );
      *(xmlLink->GetFromPort()) = static_cast< long int >( links[i].GetFromPort() );
      *(xmlLink->GetToPort()) = static_cast< long int >( links[i].GetToPort() );

      //Try to store link cons,
      //link cons are (x,y) wxpoint
      //here I store x in one vector and y in the other
      for ( size_t j = 0; j < links[ i ].GetNumberOfPoints(); ++j )
	   {
         xmlLink->GetLinkPoint( j )->SetPoint( std::pair< unsigned int, unsigned int >( links[ i ].GetPoint( j )->x, links[ i ].GetPoint( j )->y ) );
      }
   }

   //  Models
   std::map< int, Module >::iterator iter;
   for ( iter=modules.begin(); iter!=modules.end(); ++iter )
   {
      modules[ iter->first ].GetPlugin()->SetID( iter->first );
      nodes.push_back( 
                  std::pair< VE_XML::XMLObject*, std::string >( 
                  modules[ iter->first ].GetPlugin()->GetVEModel(), "veModel" ) 
                     );
      dynamic_cast< VE_Model::Model* >( nodes.back().first )->SetModelName( modules[ iter->first ].GetClassName() );
   }

   //  tags
   /*for ( size_t i = 0; i < veTagVector.size(); ++i )
   {
      delete veTagVector.at( i );
   }
   veTagVector.clear();

   for ( size_t i = 0; i < tags.size(); ++i )
   {
      std::pair< unsigned int, unsigned int > pointCoords;

      veTagVector.push_back( new VE_Model::Tag( doc ) );

      veTagVector.back()->SetTagText( tags.back().text.c_str() );

      pointCoords.first = tags.back().cons[0].x;
      pointCoords.second = tags.back().cons[0].y;
      veTagVector.back()->GetTagPoint( 0 )->SetPoint( pointCoords );

      pointCoords.first = tags.back().cons[1].x;
      pointCoords.second = tags.back().cons[1].y;
      veTagVector.back()->GetTagPoint( 1 )->SetPoint( pointCoords );

      pointCoords.first = tags.back().box.x;
      pointCoords.second = tags.back().box.y;
      veTagVector.back()->GetTagPoint( 2 )->SetPoint( pointCoords );
   }

   for ( size_t i = 0; i < tags.size(); ++i )
   {
      doc->getDocumentElement()->appendChild
         ( 
            veTagVector.at( i )->GetXMLData( "veTag" )
         );
   }

   VE_XML::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();
   netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );

   return fileName;
}*/
}

