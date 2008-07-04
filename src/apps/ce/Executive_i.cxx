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
#include <apps/ce/Executive_i.h>

#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/DataValuePairPtr.h>

#include <apps/ce/Execute_Thread.h>
#include <apps/ce/QueryThread.h>

#include <ace/OS.h>

#include <boost/lexical_cast.hpp>

#include <iostream>
#include <fstream>
#include <sstream>

using namespace ves::open::xml;
//using namespace ves::ce::util;
using namespace VE_CE::Utilities;
// Implementation skeleton constructor
Body_Executive_i::Body_Executive_i( CosNaming::NamingContext_ptr nc )
        : naming_context_( CosNaming::NamingContext::_duplicate( nc ) )
{
    _network = new Network();
    _scheduler = new Scheduler( _network );

    //Initialize all the XML objects
    XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new XMLCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new shader::ShaderCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Model", new model::ModelCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new cad::CADCreator() );
}

// Implementation skeleton destructor
Body_Executive_i::~Body_Executive_i( void )
{
    delete _network;
    delete _scheduler;
}

char * Body_Executive_i::GetImportData(
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    std::cout << "VE-CE : GetImportData " << module_id << " " << port_id << std::endl;

    Module *mod = _network->GetModule( _network->moduleIdx( module_id ) );
    if( !mod )
    {
        std::cerr << "Cannot find module, id# " << module_id << std::endl;
        return CORBA::string_dup( "" );
    }

    IPort *iport = mod->getIPort( mod->iportIdx( port_id ) );

    //bool        rv = false;
    std::string str;

    if( iport && iport->nconnections() )
    {
        Connection* conn = iport->connection( 0 ); // should only have one connection
        OPort* oport = conn->get_oport();

        if( oport->have_data() )
        {
            /*Package p;
            p.SetSysId("temp.xml");
            p.intfs.push_back(oport->_data);

            str = p.Save(rv);*/
            std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
            nodes.push_back( std::pair< CommandPtr, std::string  >( oport->GetPortData(), std::string( "vecommand" ) ) );
            std::string fileName( "returnString" );
            XMLReaderWriter netowrkWriter;
            netowrkWriter.UseStandaloneDOMDocumentManager();
            netowrkWriter.WriteXMLDocument( nodes, fileName, "Command" );
            str = fileName;
        }
        else
        {
            std::string msg = "Mod #"
                              + boost::lexical_cast<std::string>( module_id )
                              + " IPort, id #"
                              + boost::lexical_cast<std::string>( port_id )
                              + " has no data\n" ;
            ClientMessage( msg.c_str() );
        }
    }
    else
    {
        std::string msg = "Unable to get mod #"
                          + boost::lexical_cast<std::string>( module_id )
                          + " IPort, id #"
                          + boost::lexical_cast<std::string>( port_id )
                          + "\n" ;
        ClientMessage( msg.c_str() );
    }

    _mutex.release();

    return CORBA::string_dup( str.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::execute( std::string mn )
{
    std::string msg;

    if( _exec_thread.find( mn ) == _exec_thread.end() )
    {
        std::cerr << "Cannot find execution thread for " << mn << std::endl;
    }
    else
    {
        if( !_exec_thread[mn]->needexecute() )
        {
            msg = "Failed to execute " + mn + "\n";
            //ClientMessage(msg.c_str());
        }
        else
        {
            msg = "Executing " + mn + "\n";
            //ClientMessage(msg.c_str());
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetExportData(
    CORBA::Long module_id,
    CORBA::Long port_id,
    const char * data
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( std::string( data ), "Command", "vecommand" );
    //delete data;
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    // Should only be one item. But, maybe later...
    if( !_network->GetModule( _network->moduleIdx( module_id ) )->setPortData(
                port_id, boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) ) )
       )
    {
        std::string msg = "Unable to set mod id# "
                          + boost::lexical_cast<std::string>( module_id )
                          + ", port id# "
                          + boost::lexical_cast<std::string>( port_id )
                          + "'s port data\n";
        ClientMessage( msg.c_str() );
    }
}

char * Body_Executive_i::GetExportData(
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();
    //std::cout << "VE-CE : GetExportData "<< module_id << " " << port_id << std::endl;

    //Interface intf;
    CommandPtr portData( new Command() );
    if( !_network->GetModule( _network->moduleIdx( module_id ) )->getPortData( port_id, portData ) )
    {
        std::string msg = "Mod #"
                          + boost::lexical_cast<std::string>( module_id )
                          + " IPort, id #"
                          + boost::lexical_cast<std::string>( port_id )
                          + " has no data\n" ;
        ClientMessage( msg.c_str() );
        _mutex.release();
        return CORBA::string_dup( "" );
    }

    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< CommandPtr, std::string  >( portData, std::string( "vecommand" ) ) );
    std::string fileName( "returnString" );
    XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Command" );

    //bool        rv;
    //std::string str;

    //Package p;
    //p.SetSysId("temp.xml");
    //p.intfs.push_back(intf);

    //str = p.Save(rv);

    _mutex.release();

    //if(rv)
    return CORBA::string_dup( fileName.c_str() );
}

void Body_Executive_i::SetProfileData(
    CORBA::Long module_id,
    CORBA::Long port_id,
    const Types::Profile & data
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    std::cout << "VE-CE : SetProfileData " << module_id << " " << port_id << std::endl;


    if( !_network->GetModule( _network->moduleIdx( module_id ) )->setPortProfile( port_id, &data ) )
    {
        std::string msg = "Unable to set mod id# "
                          + boost::lexical_cast<std::string>( module_id )
                          + ", port id# "
                          + boost::lexical_cast<std::string>( port_id )
                          + "'s port profile\n";
        ClientMessage( msg.c_str() );
    }
    else
    {
        ; //std::cout << "SetPortProfile success\n";
    }

    _mutex.release();
}

void Body_Executive_i::GetProfileData(
    CORBA::Long module_id,
    CORBA::Long port_id,
    Types::Profile_out data
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    std::cout << "VE-CE : GetProfileData " << module_id << " " << port_id << std::endl;

    Module *mod = _network->GetModule( _network->moduleIdx( module_id ) );
    if( !mod )
    {
        std::cerr << "VE-CE : Cannot find module, id# " << module_id << std::endl;
        return;
    }
    IPort *iport = mod->getIPort( mod->iportIdx( port_id ) );

    if( iport && iport->nconnections() )
    {
        Connection* conn = iport->connection( 0 ); // should only have one connection
        OPort* oport = conn->get_oport();

        if( oport->have_profile() )
        {
            data = new Types::Profile( *( oport->_profile ) );
        }
        else
        {
            std::string msg = "Mod #"
                              + boost::lexical_cast<std::string>( module_id )
                              + " IPort, id #"
                              + boost::lexical_cast<std::string>( port_id )
                              + " has no Profile\n" ;
            std::cerr << msg;
            ClientMessage( msg.c_str() );
        }
    }
    else
    {
        std::string msg = "Unable to get Profile from mod #"
                          + boost::lexical_cast<std::string>( module_id )
                          + " IPort, id #"
                          + boost::lexical_cast<std::string>( port_id )
                          + "\n" ;
        std::cerr << msg;
        ClientMessage( msg.c_str() );
    }

    _mutex.release();
}
/////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::execute_next_mod( long module_id )
{
    int moduleIndex = _network->moduleIdx( module_id );
    if( moduleIndex < 0 )
    {
        std::cerr << "Unit ID " << module_id << " cannot be found in the list"
            << " of available units." << std::endl;
        return;
    }
    
    //module_id is the currently active module_id and this function
    //is determining which is the next module to execute
    //id is module jsut executed
    std::string msg( "" );

    std::string mod_type = _network->GetModule( moduleIndex )->GetModuleName();
    if( _mod_units.find( mod_type ) != _mod_units.end() )
    {
        try
        {
            _mod_units[ mod_type ]->_non_existent();
            msg = _mod_units[ mod_type ]->GetStatusMessage();
        }
        catch ( CORBA::Exception & )
        {
            std::cerr << "Cannot contact module id " << module_id
            << " name " << mod_type << std::endl;
            return;
        }
    }
    else
    {
        std::cerr << "Module name " << mod_type << " is not yet connected." << std::endl;
        return;
    }

    if( msg == "" )
    {
        return;
    }
    
    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( msg, "Command", "vecommand" );
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    if( objectVector.empty() )
    {
        return;
    }

    CommandPtr returnState =  boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );

    long rs;
    // 0:O.K, 1:ERROR, 2:?, 3:FB COMLETE
    returnState->GetDataValuePair( "RETURN_STATE" )->GetData( rs );

    if( rs == -1 )
    {
        returnState->GetDataValuePair( "return_state" )->GetData( rs );
    }
    //delete the object vector
    objectVector.clear();

    _network->GetModule( moduleIndex )->_return_state = rs;
    //If we have an error - see above
    if( rs == 1 )
    {
        return;
    }

    //rt counts down from n to -1 where -1 is the last module to be exectued.
    //rt is the next module index which is why -1 is subtracted
    int rt = _scheduler->execute( _network->GetModule( moduleIndex ) ) - 1;
    std::cout << "VE-CE::execute_next_mod rt " 
        << rt << " module id " << module_id << std::endl;
    //This onyl works for serial exectuion and for units with 1 input and output port
    int previousModuleIndex = rt + 1;
    if( rt < 0 )
    {
        ClientMessage( "VE-Suite Network Execution Complete\n" );
        return;
    }
    
    if( _mod_units.find( _network->GetModule( rt )->GetModuleName() ) == 
       _mod_units.end() )
    {
        std::cerr <<  "VE-CE : Cannot find running unit " 
            << _network->GetModule( rt )->GetModuleName() << std::endl
            << "The units that are registerd are " << std::endl;
        for( std::map< std::string, Body::Unit_var >::const_iterator iter = 
            _mod_units.begin(); iter != _mod_units.end(); ++iter )
        {
            std::cout << "Unit name = " << iter->first << std::endl;
        }
        return;
    }

    //call query on previous module with "Get XML Model Results"
    CommandPtr resultsCommand( new Command() );
    resultsCommand->SetCommandName( "Get XML Model Results" );
    //Get module id and unit name
    //gets tagged as vendorUnit (module name) and moduleId (number id not uuid)
    DataValuePairPtr vendorData( new DataValuePair() );
    vendorData->SetData( "vendorUnit", _network->
        GetModule( previousModuleIndex )->GetModuleName() );
    resultsCommand->AddDataValuePair( vendorData );
    DataValuePairPtr moduleIdData( new DataValuePair() );
    moduleIdData->SetData( "moduleId", static_cast< long int >( 
        _network->GetModule( previousModuleIndex )->get_id() ) );
    resultsCommand->AddDataValuePair( moduleIdData );
    //Then parse command
    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< CommandPtr, std::string  >(
        resultsCommand, std::string( "vecommand" ) ) );
    std::string fileName( "returnString" );
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.WriteXMLDocument( nodes, fileName, "Command" );
    //Now query the unit for data
    std::string unitResultsData = Query( fileName.c_str() );
    //std::cout << " results test = " << std::endl << unitResultsData << std::endl;
    nodes.clear();
    if( unitResultsData != "NULL" )
    {
        XMLReaderWriter networkReader;
        networkReader.UseStandaloneDOMDocumentManager();
        networkReader.ReadFromString();
        networkReader.ReadXMLData( unitResultsData, "Command", "vecommand" );
        objectVector = networkReader.GetLoadedXMLObjects();
    
        //Then set inputs on next module
        nodes.push_back( std::pair< CommandPtr, std::string  >(
            boost::dynamic_pointer_cast< ves::open::xml::Command >( 
            objectVector.at( 0 ) ), std::string( "vecommand" ) ) );
    }
    //Now get the input data for the current model
    const std::vector< CommandPtr > inputList = 
        _network->GetModule( rt )->GetVEModel()->GetInputs();
    for( size_t k = 0; k < inputList.size(); ++k )
    {
        nodes.push_back( std::pair< CommandPtr, std::string  >(
            inputList.at( k ), std::string( "vecommand" ) ) );
    }
    
    fileName.assign( "returnString" );
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.WriteXMLDocument( nodes, fileName, "Command" );
    //std::cout << fileName << std::endl;
    try
    {
        long int tempID = 
            static_cast< long int >( _network->GetModule( rt )->get_id() );
        _mod_units[ _network->GetModule( rt )->GetModuleName() ]->
            SetCurID( tempID );
        if( fileName != "NULL" )
        {
            _mod_units[ _network->GetModule( rt )->GetModuleName()]->
                SetParams( tempID, fileName.c_str() );
        }
        execute( _network->GetModule( rt )->GetModuleName() );
    }
    catch ( CORBA::Exception & )
    {
        std::cerr << "VE-CE : Cannot contact Module " << module_id << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetModuleMessage(
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // send a unit message to all uis
    //std::string message = std::string( "SetModuleMessage ") + std::string( msg );
    std::string message = std::string( msg );
    //_mutex.acquire();
    ClientMessage( message.c_str() );
    //_mutex.release();
}
////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetModuleResult(
    CORBA::Long module_id,
    const char * result
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( std::string( result ), "Command", "vecommand" );
    //delete result;
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    _network->GetModule( _network->moduleIdx( module_id ) )->SetResultsData( objectVector );

    std::string msg = "Mod id# "
                      + boost::lexical_cast<std::string>( module_id )
                      + "'s Execution is done\n";
    ClientMessage( msg.c_str() );

    _mutex.release();
}
////////////////////////////////////////////////////////////////////////////
char * Body_Executive_i::GetModuleResult(
    CORBA::Long module_id
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    std::cout << "VE-CE : Body_Executive_i::GetModuleResult has been replaced with the query function." << std::endl;
    return CORBA::string_dup( "" );
}
////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetNetwork(
    const char * network
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();
    //Clear old network and schedule
    _network->clear();
    _scheduler->clear();

    // Keep track of power requirements
    //_module_powers.clear();
    //_thermal_input.clear();
    std::string strNetwork( network );

    if( _network->parse( strNetwork ) )
    {
        _mutex.release();
        // Make the new schedule
        if( !_scheduler->schedule( 0 ) )
        {
            ClientMessage( "Problem in VE-Suite Schedule\n" );
            return;
        }
        else
        {
            ClientMessage( "Successfully Scheduled VE-Suite Network\n" );
            _scheduler->print_schedule();
        }
    }
    else
    {
        _mutex.release();
        ClientMessage( "Problem in VE-CE SetNetwork\n" );
        return;
    }
}
////////////////////////////////////////////////////////////////////////////
char * Body_Executive_i::GetNetwork(
    const char * moduleName
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();
    std::string xmlNetwork = _network->GetNetworkString();
    _mutex.release();

    if( xmlNetwork.empty() )
    {
        uis_[ moduleName ]->Raise( "No Current VE-Suite Network Present In VE-CE.\n" );
        //ClientMessage( "No Current VES Network Present In VE-CE.\n" );
    }

    return CORBA::string_dup( xmlNetwork.c_str() );
}
////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetModuleUI(
    CORBA::Long module_id,
    const char * ui
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    ///I don't think this function is used. We may be able to remove it.
    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( std::string( ui ), "Command", "vecommand" );
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    _network->GetModule( _network->moduleIdx( module_id ) )->SetInputData( objectVector );
    _network->GetModule( _network->moduleIdx( module_id ) )->_need_execute = 1;
    _network->GetModule( _network->moduleIdx( module_id ) )->_return_state = 0;

    _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetWatchList(
    const Types::ArrayLong & id
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    watch_list_ = id;

    _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
::Types::ArrayLong * Body_Executive_i::GetWatchList(
    ACE_ENV_SINGLE_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    // Stuff here

    _mutex.release();

    return new ::Types::ArrayLong( watch_list_ );
}
////////////////////////////////////////////////////////////////////////////////
char * Body_Executive_i::GetStatus(
    ACE_ENV_SINGLE_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    // Stuff here

    _mutex.release();

    return CORBA::string_dup( "" );//str.c_str());//"yang";//0;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::StartCalc(
    ACE_ENV_SINGLE_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _scheduler->reset();

    if( _scheduler->snodes_size() == 0 )
    {
        return;
    }

    _mutex.acquire();

    int rt = _scheduler->execute( 0 ) - 1;

    if( rt < 0 )
    {
        std::cerr << "VES Network Execution Complete" << std::endl;
    }
    else
    {
        std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
        const std::vector< CommandPtr > inputList = _network->GetModule( rt )->GetVEModel()->GetInputs();
        for( size_t k = 0; k < inputList.size(); ++k )
        {
            nodes.push_back( std::pair< CommandPtr, std::string  >(
                                 inputList.at( k ), std::string( "vecommand" ) )
                           );
        }

        std::string fileName( "returnString" );
        XMLReaderWriter netowrkWriter;
        netowrkWriter.UseStandaloneDOMDocumentManager();
        netowrkWriter.WriteXMLDocument( nodes, fileName, "Command" );

        try
        {
            ClientMessage( "Initial Execute\n" );
            if( !_mod_units.empty() )
            {
                std::string moduleName = _network->GetModule( rt )->GetModuleName();
                if( _mod_units.find( moduleName ) != _mod_units.end() )
                {
                    //This must be called because the raw model is not passed
                    //to the unit anymore. Only the input variables are passed
                    //to the unit. This should be changed in the future so 
                    //that the veopen model data is passed directly to
                    //the respective unit.
                    long int tempID = static_cast< long >( _network->GetModule( rt )->get_id() );
                    _mod_units[ moduleName ]->SetParams( tempID, fileName.c_str() );
                    _mod_units[ moduleName ]->SetCurID( tempID );
                    // This starts a chain reaction which eventually leads to Execute_Thread
                    // which calls executenextmod in this class
                    // by having the thread do that all subsequent modules get executed
                    execute( moduleName );
                }
                else
                {
                    std::cerr << "Initial Execute, module " << moduleName
                        << " is not registered yet" << std::endl;
                }
            }
            else
            {
                ClientMessage( "No Module Units connected to the VE-CE, skipping execution\n" );
            }
        }
        catch ( CORBA::Exception & )
        {
            std::cerr << "Initial Execute, cannot contact Module "
                << _network->GetModule( rt )->GetModuleName() << std::endl;
        }
    }

    _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::StopCalc(
    ACE_ENV_SINGLE_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();
    // Stop all units
    std::map<std::string, Body::Unit_var>::iterator iter;
    for( iter = _mod_units.begin(); iter != _mod_units.end(); )
    {
        try
        {
            //queryString.append( iter->second->Query() );
            iter->second->StopCalc();
            ++iter;
        }
        catch ( CORBA::Exception & )
        {
            // std::cout << iter->first <<" is obsolete." << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            UnRegisterUnit( iter->first.c_str() );
            // Not sure if increment here or not
            _mod_units.erase( iter++ );
        }
    }
    _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::PauseCalc(
    ACE_ENV_SINGLE_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();
    // Pause all units
    std::map<std::string, Body::Unit_var>::iterator iter;
    for( iter = _mod_units.begin(); iter != _mod_units.end(); )
    {
        try
        {
            //queryString.append( iter->second->Query() );
            iter->second->PauseCalc();
            ++iter;
        }
        catch ( CORBA::Exception & )
        {
            // std::cout << iter->first <<" is obsolete." << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            UnRegisterUnit( iter->first.c_str() );
            // Not sure if increment here or not
            _mod_units.erase( iter++ );
        }
    }
    _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::Resume(
    ACE_ENV_SINGLE_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();
    // Resume all the modules
    std::map<std::string, Body::Unit_var>::iterator iter;
    for( iter = _mod_units.begin(); iter != _mod_units.end(); )
    {
        try
        {
            //queryString.append( iter->second->Query() );
            iter->second->Resume();
            ++iter;
        }
        catch ( CORBA::Exception & )
        {
            // std::cout << iter->first <<" is obsolete." << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            UnRegisterUnit( iter->first.c_str() );
            // Not sure if increment here or not
            _mod_units.erase( iter++ );
        }
    }
    _mutex.release();
}

char *  Body_Executive_i::Query( const char * command
                                 ACE_ENV_SINGLE_ARG_DECL
                               )
ACE_THROW_SPEC(( CORBA::SystemException, Error::EUnknown ) )
{
//std::cout << command << std::endl;
    _mutex.acquire();
    // read the command to get the module name and module id
    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( command, "Command", "vecommand" );
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    std::string moduleName;
    std::string vendorUnit;
    unsigned int moduleId = 0;
    CommandPtr tempCommand =  boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
    CommandPtr passCommand( new Command() );
    passCommand->SetCommandName( tempCommand->GetCommandName() );
    size_t numDVP = tempCommand->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numDVP; ++i )
    {
        DataValuePairPtr tempPair = tempCommand->GetDataValuePair( i );
        /*if ( tempPair->GetDataName() == "moduleName" )
        {
           tempPair->GetData( moduleName );
        }
        else */
        if( tempPair->GetDataName() == "moduleId" )
        {
            tempPair->GetData( moduleId );
        }
        else if( tempPair->GetDataName() == "vendorUnit" )
        {
            tempPair->GetData( vendorUnit );
        }
        else
        {
            // the copy constructor is needed here because
            // the tempCommand owns the memory as well as the passcommand
            // each of these commands would then delete the same memory
            passCommand->AddDataValuePair( tempPair );
        }
    }
    //delete the object vector
    objectVector.clear();

    ///string used to hold query data
    std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
    nodes.push_back(
        std::pair< XMLObjectPtr, std::string >( passCommand, "vecommand" )
    );

    XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    // Resume all the modules
    std::map<std::string, Body::Unit_var>::iterator iter;
    if( !vendorUnit.empty() )
    {
        //find the unit the normal way
        iter = _mod_units.find( vendorUnit );
    }
    else
    {
        // if we are doing an aspen type query and do not have
        // a unit name or module id
        //std::cout << "aspen query" << std::endl;
        iter = _mod_units.begin();
    }

    if( iter == _mod_units.end() )
    {
        std::cout << "VE-CE : No units to query" << std::endl;
        _mutex.release();
        return 0;
    }

    std::string queryString;

    std::map< std::string, QueryThread* >::iterator queryIter;
    queryIter = queryThreads.find( iter->first );
    queryIter->second->QueryData( status, moduleId );

    std::string returnString;
    while (( returnString != "NULL" ) && returnString.empty() )
    {
        ACE_OS::sleep( 1 );
        returnString = queryIter->second->GetQueryData();
    }
    //queryString.assign( queryIter->second->GetQueryData() );
    queryString.assign( returnString );
    _mutex.release();

    return CORBA::string_dup( queryString.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::RegisterUI(
    const char * UIName,
    Body::UI_ptr ui_ptr
    ACE_ENV_ARG_DECL
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    std::string tempName( UIName );
    //delete UIName;
    try
    {
        Body::UI_var ui = Body::UI::_duplicate( ui_ptr );
        if( CORBA::is_nil( ui ) )
            std::cerr << "NULL UI" << std::endl;

        uis_[ tempName ] = ui;

        _mutex.release();
        std::string msg = tempName + " Connected to VE-CE\n";
        ui->Raise( msg.c_str() );
        //ClientMessage( msg.c_str() );
        std::cerr << tempName << " : registered a UI" << std::endl;
    }
    catch ( CORBA::Exception &ex )
    {
        std::cerr << "Can't call UI name " << tempName
        << " " << ex._info().c_str() << std::endl;
        _mutex.release();
    }
    return;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::RegisterUnit(
    const char * UnitName,
    Body::Unit_ptr unit,
    CORBA::Long flag
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    // When this is called, a unit is already binded to the name service,
    // so this call can get it's reference from the name service
    std::string strUnitName( UnitName );
    //delete UnitName;

    std::string message =  std::string( "Going to RegisterUnit " ) + strUnitName + std::string( "\n" );
    ClientMessage( message.c_str() );

    _mod_units[ strUnitName ] = Body::Unit::_duplicate( unit );

    std::map<std::string, Execute_Thread*>::iterator iter;
    iter = _exec_thread.find( strUnitName );

    if( iter == _exec_thread.end() )
    {
        // CLEAN THIS UP IN UNREGISTER UNIT !
        Execute_Thread *ex = new Execute_Thread( _mod_units[ strUnitName ], ( Body_Executive_i* )this );
        ex->activate();
        _exec_thread[ strUnitName ] = ex;
    }
    else //replace it with new reference
    {
        ACE_Task_Base::cleanup( iter->second, NULL );
        if( iter->second )
        {
            delete iter->second;
        }
        Execute_Thread *ex = new Execute_Thread( _mod_units[ strUnitName ], ( Body_Executive_i* )this );
        ex->activate();
        _exec_thread[ strUnitName ] = ex;
    }

    std::map<std::string, QueryThread* >::iterator iterQuery;
    iterQuery = queryThreads.find( strUnitName );

    if( iterQuery == queryThreads.end() )
    {
        QueryThread* query = new QueryThread( _mod_units[ strUnitName ] );
        query->activate();
        queryThreads[ strUnitName ] = query;
    }
    else
    {
        ACE_Task_Base::cleanup( iterQuery->second, NULL );
        if( iterQuery->second )
        {
            delete iterQuery->second;
        }
        QueryThread* query = new QueryThread( _mod_units[ strUnitName ] );
        query->activate();
        queryThreads[ strUnitName ] = query;
    }

    message = std::string( "Successfully registered " ) + strUnitName + std::string( "\n" );
    ClientMessage( message.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::UnRegisterUI(
    const char * UIName
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    std::map<std::string, Body::UI_var>::iterator iter;
    _mutex.acquire();
    // Add your implementation here
    std::string strUI( UIName );
    //delete UIName;
    iter = uis_.find( strUI );
    if( iter != uis_.end() )
    {
        uis_.erase( iter );
        std::cout << "VE-CE : " << strUI << " Unregistered!\n";
    }
    _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::UnRegisterUnit(
    const char * UnitName
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    std::map<std::string, Execute_Thread*>::iterator iter;
    iter = _exec_thread.find( std::string( UnitName ) );
    if( iter != _exec_thread.end() )
    {
        ACE_Task_Base::cleanup( iter->second, NULL );
        //if( iter->second )
        //    delete iter->second;

        _exec_thread.erase( iter );
        std::cout << "VE-CE : " << UnitName << " Unregistered!\n";
    }

    std::map< std::string, QueryThread* >::iterator iterQuery;
    iterQuery = queryThreads.find( std::string( UnitName ) );
    if( iterQuery != queryThreads.end() )
    {
        ACE_Task_Base::cleanup( iterQuery->second, NULL );
        //if( iterQuery->second )
        //    delete iterQuery->second;

        queryThreads.erase( iterQuery );
        std::cout << "VE-CE : " << UnitName << " Query Unregistered!\n";
    }

    _mutex.release();

}
////////////////////////////////////////////////////////////////////////////////
CORBA::Long Body_Executive_i::GetGlobalMod(
    Types::ArrayLong_out ids
)
ACE_THROW_SPEC((
                   CORBA::SystemException
                   , Error::EUnknown
               ) )
{
    _mutex.acquire();

    // Add your implementation here

    _mutex.release();

    return CORBA::Long( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetID(
    const char * moduleName,
    ::CORBA::Long id
)
ACE_THROW_SPEC((
                   ::CORBA::SystemException,
                   ::Error::EUnknown
               ) )
{
    std::map< std::string, Body::Unit_var >::iterator iter;
    _mutex.acquire();

    iter = _mod_units.find( std::string( moduleName ) );
    if( iter == _mod_units.end() )
    {
        std::string msg = "Could not find " + std::string( moduleName ) + " unit\n";
        ClientMessage( msg.c_str() );
        _mutex.release();
        return;
    }

    try
    {
        iter->second->_non_existent();
        iter->second->SetID( id );
        _mutex.release();
    }
    catch ( CORBA::Exception & )
    {
        std::cout << "VE-CE : " << iter->first << " is obsolete." << std::endl;
        _mod_units.erase( iter );
        _mutex.release();
        UnRegisterUnit( moduleName );
    }
    catch ( ... )
    {
        std::cout << "VE-CE : another kind of exception " << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::DeleteModuleInstance(
    const char * moduleName,
    ::CORBA::Long module_id
)
ACE_THROW_SPEC((
                   ::CORBA::SystemException,
                   ::Error::EUnknown
               ) )
{
    // Add your implementation here
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::SetParams(
    const char * moduleName,
    ::CORBA::Long module_id,
    const char * param
)
ACE_THROW_SPEC((
                   ::CORBA::SystemException,
                   ::Error::EUnknown
               ) )
{
    // Add your implementation here
}
////////////////////////////////////////////////////////////////////////////////
void Body_Executive_i::ClientMessage( const char *msg )
{
    std::cout << "VE-CE : Output = " << msg;
    for( std::map<std::string, Body::UI_var>::iterator
            iter = uis_.begin(); iter != uis_.end(); )
    {
        std::cout << "VE-CE : " << msg << " to -> " << iter->first << std::endl;
        try
        {
            iter->second->_non_existent();
            iter->second->Raise( msg );
            ++iter;
        }
        catch ( CORBA::Exception & )
        {
            std::cout << "VE-CE : " << iter->first << " is obsolete." << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            uis_.erase( iter++ );
        }
        catch ( ... )
        {
            std::cout << "VE-CE : another kind of exception " << std::endl;
        }
    }
}
