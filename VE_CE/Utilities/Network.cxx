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
//using namespace VE_XML::VE_Model;

Network::Network ()
{
   veNetwork = 0;
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
      VE_XML::VE_Model::Model* model = dynamic_cast< VE_XML::VE_Model::Model* >( objectVector.at( i ) );
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
   if ( veNetwork )
   {
      delete veNetwork;
      veNetwork = 0;
   }
   // we are expecting that a network will be found
   if ( !objectVector.empty() )
   {
      veNetwork = dynamic_cast< VE_XML::VE_Model::Network* >( objectVector.at( 0 ) );
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

   for ( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i ) 
   {

      ///May want to do error checking here in the future
      /*
      if ( FrMod.find(*iter)==FrMod.end()  || ToMod.find(*iter)==FrMod.end()   ||
           FrPort.find(*iter)==FrMod.end() || ToPort.find(*iter)==FrMod.end()) 
      {
         std::cerr << "Bad link found\n";
         return  0;
      }
      */

      Connection* cn = new Connection( i );
      _connections.push_back( cn );

      long fromModuleID, toModuleID;
      veNetwork->GetLink( i )->GetFromModule()->GetData( fromModuleID );
      veNetwork->GetLink( i )->GetToModule()->GetData( toModuleID );

      long int toPort = *(veNetwork->GetLink( i )->GetToPort());
      long int fromPort = *(veNetwork->GetLink( i )->GetFromPort());

      GetModule( moduleIdx( toModuleID ) )->addIPort( toPort, cn );
      GetModule( moduleIdx( fromModuleID ) )->addOPort( fromPort, cn );
      ///May want to do error checking here in the future
      //{
      //   std::cerr << "Error adding ports" << std::endl;
      //   return 0;
      //}
   }
   return 1;
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

   _module_ptrs.push_back( new Module() );
}  
////////////////////////////////////////////////////////////////////////////////
std::string Network::GetNetworkString( void )
{   
   // Here we wshould loop over all of the following
   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   //  Newtork
   if ( !veNetwork )
   {
      return std::string( "" );
   }

   // Just push on the old network as ce can't modify the network
   // it only uses the network. conductor modifies the network
   nodes.push_back( 
                     std::pair< VE_XML::XMLObject*, std::string >( 
                     veNetwork, "veNetwork" ) 
                  );

   //  Models
   for ( size_t i = 0; i < _module_ptrs.size(); ++i )
   {
      nodes.push_back( 
                        std::pair< VE_XML::XMLObject*, std::string >( 
                        _module_ptrs.at( i )->GetVEModel(), "veModel" ) 
                     );
   }

   std::string fileName( "returnString" );
   VE_XML::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();
   netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
   return fileName;
}

