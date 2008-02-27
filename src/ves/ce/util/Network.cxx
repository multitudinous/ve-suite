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
#include <ves/ce/util/Network.h>
#include <ves/ce/util/Module.h>
#include <ves/ce/util/Connection.h>

#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>

#include <set>
#include <iostream>

using namespace VE_CE::Utilities;
using namespace ves::open::xml;

Network::Network()
{
    veNetwork = 0;
}
////////////////////////////////////////////////////////////////////////////////
Network::~Network()
{
    clear();
}
////////////////////////////////////////////////////////////////////////////////
void Network::clear()
{
    for( size_t i = 0; i < _module_ptrs.size(); ++i )
    {
        delete _module_ptrs[i];
    }
    _module_ptrs.clear();

    for( size_t i = 0; i < _connections.size(); ++i )
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
    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();

    // do this for models
    networkWriter.ReadXMLData( xmlNetwork, "System", "veSystem" );
    std::vector< XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();
    model::SystemPtr tempSystem = objectVector.at( 0 );
    if( !tempSystem )
    {
        std::cerr << "Improperly formated ves file."
        << "VES File Read Error" << std::endl;
        return 0;
    }

    std::vector< model::ModelPtr > models =
        tempSystem->GetModels();
    // now lets create a list of them
    for( size_t i = 0; i < models.size(); ++i )
    {
        add_module( models.at( i )->GetModelID(),
                    models.at( i )->GetModelName() );
        GetModule( i )->SetVEModel( models.at( i ) );
    }

    //read for the network info now
    //this code must be second as the connections stuff
    //assumes that the modules have already
    //been created
    //we are expecting that a network will be found
    veNetwork = tempSystem->GetNetwork();

    for( size_t i = 0; i < veNetwork->GetNumberOfLinks(); ++i )
    {
        Connection* cn = new Connection( i );
        _connections.push_back( cn );

        long fromModuleID, toModuleID;
        veNetwork->GetLink( i )->GetFromModule()->GetData( fromModuleID );
        veNetwork->GetLink( i )->GetToModule()->GetData( toModuleID );

        long int toPort = *( veNetwork->GetLink( i )->GetToPort() );
        long int fromPort = *( veNetwork->GetLink( i )->GetFromPort() );

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
int Network::nmodules()
{
    return _module_ptrs.size();
}
////////////////////////////////////////////////////////////////////////////////
int Network::GetModuleIndex( Module* mod )
{
    for( size_t i = 0; i < _module_ptrs.size(); ++i )
    {
        if( mod->get_id() == _module_ptrs[i]->get_id() )
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
    for( size_t i = 0; i < _module_ptrs.size(); ++i )
    {
        if( id == _module_ptrs[ i ]->get_id() )
            return i;
    }
    return -1;
}
////////////////////////////////////////////////////////////////////////////////
void Network::add_module( int m, std::string name )
{
    if( moduleIdx( m ) >= 0 )
        return;

    _module_ptrs.push_back( new Module() );
}
////////////////////////////////////////////////////////////////////////////////
std::string Network::GetNetworkString( void )
{
    if( !veNetwork )
    {
        return std::string( "" );
    }

    model::SystemPtr tempSystem =
        new model::System();
    //  Models
    for( size_t i = 0; i < _module_ptrs.size(); ++i )
    {
        tempSystem->AddModel( _module_ptrs.at( i )->GetVEModel() );
    }

    //  Newtork
    tempSystem->AddNetwork( veNetwork );

    // Here we wshould loop over all of the following
    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    // Just push on the old network as ce can't modify the network
    // it only uses the network. conductor modifies the network
    nodes.push_back( std::pair< XMLObjectPtr, std::string >(
                         tempSystem, "veSystem" ) );

    std::string fileName( "returnString" );
    XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );
    return fileName;
}

