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

#include <ves/ce/Body_AMH_Executive_i.h>

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

#include <ves/ce/Execute_Thread.h>

#include <ves/ce/Body_AMI_UIHandler_i.h>
#include <ves/ce/Body_AMI_UnitHandler_i.h>

#include <ves/ce/util/Network.h>
#include <ves/ce/util/Scheduler.h>
#include <ves/ce/util/Module.h>
#include <ves/ce/util/Connection.h>
#include <ves/ce/util/IPort.h>
#include <ves/ce/util/OPort.h>

#include <ace/OS.h>

///Boost includes
#include <boost/concept_check.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>
#include <fstream>
#include <sstream>

using namespace ves::open::xml;
using namespace VE_CE::Utilities;
using namespace ves::ce;

////////////////////////////////////////////////////////////////////////////////
Body_AMH_Executive_i::Body_AMH_Executive_i( PortableServer::POA_ptr poa )
    :
    m_poa( PortableServer::POA::_duplicate( poa ) ),
    m_network( new Network() ),
    m_scheduler( new Scheduler( m_network ) )
{
    //Initialize all the XML objects
    XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new XMLCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new shader::ShaderCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Model", new model::ModelCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new cad::CADCreator() );
}
////////////////////////////////////////////////////////////////////////////////
Body_AMH_Executive_i::~Body_AMH_Executive_i()
{
    delete m_network;
    delete m_scheduler;
}
////////////////////////////////////////////////////////////////////////////////
/*
void Body_AMH_Executive_i::GetImportData (
                                Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                ::CORBA::Long module_id,
                                ::CORBA::Long port_id
                                )
{
    m_mutex.acquire();

    std::cout << "VE-CE : GetImportData " << module_id << " " << port_id << std::endl;

    Module *mod = m_network->GetModule( m_network->moduleIdx( module_id ) );
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

    m_mutex.release();

    return CORBA::string_dup( str.c_str() );
}*/
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::SetModuleMessage(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    ::CORBA::Long module_id,
    const char* msg
)
{
    boost::ignore_unused_variable_warning( module_id );
    // send a unit message to all uis
    //std::string message = std::string( "SetModuleMessage ") + std::string( msg );
    //std::string message = std::string( msg );
    //m_mutex.acquire();
    //ClientMessage( message.c_str() );
    //m_mutex.release();

    std::cout << "VE-CE : Output = " << msg;
    for( std::map<std::string, Body::UI_var>::iterator
            iter = m_uiMap.begin(); iter != m_uiMap.end(); )
    {
        std::cout << "VE-CE : " << msg << " to -> " << iter->first << std::endl;
        try
        {
            // The callback handler servant instance holds on to a reference to the
            // AMH response handler. That way, it can forward the reply back to the
            // originial client after getting the reply from the inner server.
            PortableServer::ServantBase_var servant = new Body_AMI_UIHandler_i( this->m_poa.in(),
                    _tao_rh );
            PortableServer::ObjectId_var objid =
                this->m_poa->activate_object( servant.in() );
            CORBA::Object_var obj = this->m_poa->id_to_reference( objid.in() );

            ::Body::AMI_UIHandler_var cb =  ::Body::AMI_UIHandler::_narrow( obj.in() );

            // forward the request on to the inner server, with the callback handler
            // reference.

            iter->second->_non_existent();
            iter->second->sendc_Raise( cb.in(), msg );
            ++iter;
        }
        catch( CORBA::Exception& ex )
        {
            std::cout << "VE-CE : " << iter->first
                      << " is obsolete." << std::endl
                      << ex._info().c_str() << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            m_uiMap.erase( iter++ );
        }
        catch( std::exception& ex )
        {
            std::cout << "VE-CE : another kind of exception "
                      << std::endl << ex.what() << std::endl;
        }
    }

    // nothing else to do. Our client will block until the callback handler
    // forwards the reply.

}
////////////////////////////////////////////////////////////////////////////////
/*
void Body_AMH_Executive_i::SetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id,
                                  const char * result
                                  )
{
    m_mutex.acquire();

    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( std::string( result ), "Command", "vecommand" );
    //delete result;
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    m_network->GetModule( m_network->moduleIdx( module_id ) )->SetResultsData( objectVector );

    std::string msg = "Mod id# "
    + boost::lexical_cast<std::string>( module_id )
    + "'s Execution is done\n";
    ClientMessage( msg.c_str() );

    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::GetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id
                                  )
{
    boost::ignore_unused_variable_warning( module_id );
    std::cout << "VE-CE : Body_Executive_i::GetModuleResult has been replaced with the query function." << std::endl;
    return CORBA::string_dup( "" );
}*/
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::SetNetwork(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* network
)
{
    //boost::ignore_unused_variable_warning( _tao_rh );
    m_mutex.acquire();
    //Clear old network and schedule
    m_network->clear();
    m_scheduler->clear();

    // Keep track of power requirements
    //_module_powers.clear();
    //_thermal_input.clear();
    std::string strNetwork( network );

    if( m_network->parse( strNetwork ) )
    {
        m_mutex.release();
        // Make the new schedule
        if( !m_scheduler->schedule( 0 ) )
        {
            ClientMessage( "Problem in VE-Suite Schedule\n" );
            _tao_rh->SetNetwork();
            return;
        }
        else
        {
            ClientMessage( "Successfully Scheduled VE-Suite Network\n" );
            m_scheduler->print_schedule();
        }
    }
    else
    {
        m_mutex.release();
        ClientMessage( "Problem in VE-CE SetNetwork\n" );
    }
    _tao_rh->SetNetwork();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::SetModuleUI(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    ::CORBA::Long module_id,
    const char* ui
)
{
    m_mutex.acquire();

    ///I don't think this function is used. We may be able to remove it.
    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( std::string( ui ), "Command", "vecommand" );
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    m_network->GetModule( m_network->moduleIdx( module_id ) )->SetInputData( objectVector );
    m_network->GetModule( m_network->moduleIdx( module_id ) )->_need_execute = 1;
    m_network->GetModule( m_network->moduleIdx( module_id ) )->_return_state = 0;

    _tao_rh->SetModuleUI();

    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::GetNetwork(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* moduleName )
{
    m_mutex.acquire();
    std::string xmlNetwork = m_network->GetNetworkString();
    m_mutex.release();

    if( xmlNetwork.empty() )
    {
        ::Body::UI_ptr tempUI = m_uiMap[ moduleName ];
        // The callback handler servant instance holds on to a reference to the
        // AMH response handler. That way, it can forward the reply back to the
        // originial client after getting the reply from the inner server.
        PortableServer::ServantBase_var servant = new Body_AMI_UIHandler_i( this->m_poa.in(),
                _tao_rh );
        PortableServer::ObjectId_var objid =
            this->m_poa->activate_object( servant.in() );
        CORBA::Object_var obj = this->m_poa->id_to_reference( objid.in() );

        ::Body::AMI_UIHandler_var cb =  ::Body::AMI_UIHandler::_narrow( obj.in() );

        // forward the request on to the inner server, with the callback handler
        // reference.

        tempUI->_non_existent();
        tempUI->sendc_Raise( cb.in(),
                             "No Current VE-Suite Network Present In VE-CE.\n" );
    }

    _tao_rh->GetNetwork( CORBA::string_dup( xmlNetwork.c_str() ) );
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::SetWatchList(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const ::Types::ArrayLong& id
)
{
    boost::ignore_unused_variable_warning( _tao_rh );
    m_mutex.acquire();

    m_watchList = id;

    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::GetWatchList(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
)
{
    ::Types::ArrayLong_var tempArray = new ::Types::ArrayLong( m_watchList );
    _tao_rh->GetWatchList( tempArray );
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::GetStatus(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
)
{
    _tao_rh->GetStatus( "Body_AMH_Executive_i::GetStatus is not implemented" );
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::StartCalc(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
)
{
    m_scheduler->reset();

    if( m_scheduler->snodes_size() == 0 )
    {
        _tao_rh->StartCalc();
        return;
    }

    m_mutex.acquire();

    int rt = m_scheduler->execute( 0 ) - 1;

    if( rt < 0 )
    {
        std::cerr << "VES Network Execution Complete" << std::endl;
    }
    else
    {
        std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
        const std::vector< CommandPtr > inputList = m_network->GetModule( rt )->GetVEModel()->GetInputs();
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
            if( !m_modUnits.empty() )
            {
                std::string moduleName = m_network->GetModule( rt )->GetModuleName();
                if( m_modUnits.find( moduleName ) != m_modUnits.end() )
                {
                    //This must be called because the raw model is not passed
                    //to the unit anymore. Only the input variables are passed
                    //to the unit. This should be changed in the future so
                    //that the veopen model data is passed directly to
                    //the respective unit.
                    unsigned int tempID = m_network->GetModule( rt )->get_id();
                    m_modUnits[ moduleName ]->SetParams( tempID, fileName.c_str() );
                    m_modUnits[ moduleName ]->SetCurID( tempID );
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
        catch( CORBA::Exception& ex )
        {
            std::cerr << "Initial Execute, cannot contact Module "
                      << m_network->GetModule( rt )->GetModuleName()
                      << std::endl << ex._info().c_str() << std::endl;
        }
    }

    _tao_rh->StartCalc();
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::StopCalc(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
)
{
    m_mutex.acquire();
    // Stop all units
    std::map<std::string, Body::Unit_var>::iterator iter;
    for( iter = m_modUnits.begin(); iter != m_modUnits.end(); )
    {
        try
        {
            //queryString.append( iter->second->Query() );
            iter->second->StopCalc();
            ++iter;
        }
        catch( CORBA::Exception& )
        {
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            UnRegisterUnit( _tao_rh, iter->first.c_str() );
            // Not sure if increment here or not
            m_modUnits.erase( iter++ );
        }
    }
    _tao_rh->StopCalc();
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::PauseCalc(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
)
{
    m_mutex.acquire();
    // Pause all units
    std::map<std::string, Body::Unit_var>::iterator iter;
    for( iter = m_modUnits.begin(); iter != m_modUnits.end(); )
    {
        try
        {
            //queryString.append( iter->second->Query() );
            iter->second->PauseCalc();
            ++iter;
        }
        catch( CORBA::Exception& )
        {
            // std::cout << iter->first <<" is obsolete." << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            UnRegisterUnit( _tao_rh, iter->first.c_str() );
            // Not sure if increment here or not
            m_modUnits.erase( iter++ );
        }
    }
    _tao_rh->PauseCalc();
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::Resume(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
)
{
    m_mutex.acquire();
    // Resume all the modules
    std::map<std::string, Body::Unit_var>::iterator iter;
    for( iter = m_modUnits.begin(); iter != m_modUnits.end(); )
    {
        try
        {
            //queryString.append( iter->second->Query() );
            iter->second->Resume();
            ++iter;
        }
        catch( CORBA::Exception& )
        {
            // std::cout << iter->first <<" is obsolete." << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            UnRegisterUnit( _tao_rh, iter->first.c_str() );
            // Not sure if increment here or not
            m_modUnits.erase( iter++ );
        }
    }
    _tao_rh->Resume();
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::RegisterUI(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* UIName,
    ::Body::UI_ptr ui
)
{
    m_mutex.acquire();

    std::string tempName( UIName );
    try
    {
        ::Body::UI_var tempUI = ::Body::UI::_duplicate( ui );
        if( CORBA::is_nil( tempUI ) )
        {
            std::cerr << "NULL UI" << std::endl;
        }

        m_uiMap[ tempName ] = tempUI;

        m_mutex.release();
        std::string msg = tempName + " Connected to VE-CE\n";

        // The callback handler servant instance holds on to a reference to the
        // AMH response handler. That way, it can forward the reply back to the
        // originial client after getting the reply from the inner server.
        PortableServer::ServantBase_var servant = new Body_AMI_UIHandler_i( this->m_poa.in(),
                _tao_rh );
        PortableServer::ObjectId_var objid =
            this->m_poa->activate_object( servant.in() );
        CORBA::Object_var obj = this->m_poa->id_to_reference( objid.in() );

        ::Body::AMI_UIHandler_var cb =  ::Body::AMI_UIHandler::_narrow( obj.in() );

        // forward the request on to the inner server, with the callback handler
        // reference.

        tempUI->_non_existent();
        tempUI->sendc_Raise( cb.in(), msg.c_str() );
        std::cout << "VE-CE : " << tempName << " : registered a UI" << std::endl;
    }
    catch( CORBA::Exception& ex )
    {
        std::cerr << "VE-CE : Can't call UI name " << tempName
                  << " " << ex._info().c_str() << std::endl;
        m_mutex.release();
    }
    _tao_rh->RegisterUI();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::UnRegisterUI(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* UIName
)
{
    //boost::ignore_unused_variable_warning( _tao_rh );
    std::map<std::string, Body::UI_var>::iterator iter;
    m_mutex.acquire();
    // Add your implementation here
    std::string strUI( UIName );
    //delete UIName;
    iter = m_uiMap.find( strUI );
    if( iter != m_uiMap.end() )
    {
        m_uiMap.erase( iter );
        std::cout << "VE-CE : " << strUI << " Unregistered!\n";
    }
    _tao_rh->UnRegisterUI();
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::UnRegisterUnit(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* UnitName
)
{
    m_mutex.acquire();
    const std::string unitNameStr( UnitName );
    std::map<std::string, Execute_Thread*>::iterator iter;
    iter = m_execThread.find( unitNameStr );
    std::string message =  std::string( "Going to unregister unit " ) +
                           UnitName + std::string( "\n" );
    ClientMessage( message.c_str() );
    if( iter != m_execThread.end() )
    {
        ACE_Task_Base::cleanup( iter->second, NULL );
        delete iter->second;
        m_execThread.erase( iter );
    }

    message = std::string( "Successfully unregistered " ) + unitNameStr + std::string( "\n" );
    ClientMessage( message.c_str() );
    _tao_rh->UnRegisterUnit();
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::RegisterUnit(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* UnitName,
    ::Body::Unit_ptr unit,
    ::CORBA::Long flag
)
{
    //boost::ignore_unused_variable_warning( _tao_rh );
    boost::ignore_unused_variable_warning( flag );
    // When this is called, a unit is already binded to the name service,
    // so this call can get it's reference from the name service
    const std::string strUnitName( UnitName );
    //delete UnitName;

    std::string message =  std::string( "Going to register unit " ) + strUnitName + std::string( "\n" );
    ClientMessage( message.c_str() );

    m_modUnits[ strUnitName ] = Body::Unit::_duplicate( unit );

    std::map<std::string, Execute_Thread*>::iterator iter;
    iter = m_execThread.find( strUnitName );

    if( iter == m_execThread.end() )
    {
        // CLEAN THIS UP IN UNREGISTER UNIT !
        Execute_Thread* ex = new Execute_Thread( m_modUnits[ strUnitName ], this );
        ex->activate();
        m_execThread[ strUnitName ] = ex;
    }
    else //replace it with new reference
    {
        ACE_Task_Base::cleanup( iter->second, NULL );
        delete iter->second;
        Execute_Thread* ex = new Execute_Thread( m_modUnits[ strUnitName ], this );
        ex->activate();
        m_execThread[ strUnitName ] = ex;
    }

    message = std::string( "Successfully registered " ) + strUnitName + std::string( "\n" );
    ClientMessage( message.c_str() );
    _tao_rh->RegisterUnit();
}
////////////////////////////////////////////////////////////////////////////////
/*    void Body_AMH_Executive_i::GetGlobalMod (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                               )
{
    boost::ignore_unused_variable_warning( ids );

    return CORBA::Long( 0 );
}*/
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::Query(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* commands
)
{
    //boost::ignore_unused_variable_warning( _tao_rh );
    m_mutex.acquire();

    // read the command to get the module name and module id
    XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( commands, "Command", "vecommand" );
    std::vector< XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    std::string vendorUnit;
    unsigned int moduleId;
    CommandPtr tempCommand =  boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );

    //if requesting unit list - send a list of the currently connected units
    if( tempCommand->GetCommandName().compare( "GetUnitList" ) == 0 )
    {
        CommandPtr returnState( new Command() );
        returnState->SetCommandName( "UnitList" );
        DataValuePairPtr data( new DataValuePair() );
        std::vector< std::string > list;

        //loop over units
        for( std::map< std::string, Body::Unit_var >::const_iterator iter =
                    m_modUnits.begin(); iter != m_modUnits.end(); ++iter )
        {
            list.push_back( iter->first );
        }

        data->SetData( "List",  list );
        returnState->AddDataValuePair( data );

        std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
        nodes.push_back( std::pair< XMLObjectPtr, std::string >( returnState, "vecommand" ) );
        XMLReaderWriter commandWriter;
        std::string status = "returnString";
        commandWriter.UseStandaloneDOMDocumentManager();
        commandWriter.WriteXMLDocument( nodes, status, "Command" );

        //return the list
        _tao_rh->Query( status.c_str() );
        return;
    }

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
        iter = m_modUnits.find( vendorUnit );
    }
    else
    {
        // if we are doing an aspen type query and do not have
        // a unit name or module id
        //std::cout << "aspen query" << std::endl;
        iter = m_modUnits.begin();
    }

    if( iter == m_modUnits.end() )
    {
        std::cout << "VE-CE : No units to query" << std::endl;
        m_mutex.release();
        std::string testString( "NULL" );
        _tao_rh->Query( testString.c_str() );
        return;
    }


    // The callback handler servant instance holds on to a reference to the
    // AMH response handler. That way, it can forward the reply back to the
    // originial client after getting the reply from the inner server.
    PortableServer::ServantBase_var servant =
        new ves::ce::Body_AMI_UnitHandler_i( m_poa.in(), _tao_rh );
    PortableServer::ObjectId_var objid =
        this->m_poa->activate_object( servant.in() );
    CORBA::Object_var obj = this->m_poa->id_to_reference( objid.in() );

    ::Body::AMI_UnitHandler_var cb = ::Body::AMI_UnitHandler::_narrow( obj.in() );

    // forward the request on to the inner server, with the callback handler
    // reference.

    try
    {
        iter->second->_non_existent();
        iter->second->SetCurID( moduleId );
        iter->second->sendc_Query( cb.in(), CORBA::string_dup( status.c_str() ) );
    }
    catch( CORBA::Exception& ex )
    {
        std::cout << "VE-CE : Module Query Messed up." << std::endl;
        std::cerr << "VE-CE : CORBA exception raised: " << ex._name() << std::endl;
        std::cerr << ex._info().c_str() << std::endl;
        std::string testString( "NULL" );
        _tao_rh->Query( testString.c_str() );
    }
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::SetID(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* moduleName,
    ::CORBA::Long id
)
{
    //boost::ignore_unused_variable_warning( _tao_rh );
    std::map< std::string, Body::Unit_var >::iterator iter;
    m_mutex.acquire();

    iter = m_modUnits.find( std::string( moduleName ) );
    if( iter == m_modUnits.end() )
    {
        std::string msg = "Could not find " + std::string( moduleName ) + " unit\n";
        ClientMessage( msg.c_str() );
        m_mutex.release();
        return;
    }

    try
    {
        iter->second->_non_existent();
        iter->second->SetID( id );
        _tao_rh->SetID();
    }
    catch( CORBA::Exception& )
    {
        std::cout << "VE-CE : " << iter->first << " is obsolete." << std::endl;
        m_modUnits.erase( iter );
        UnRegisterUnit( _tao_rh, moduleName );
    }
    catch( ... )
    {
        _tao_rh->SetID();
        std::cout << "VE-CE : another kind of exception " << std::endl;
    }
    m_mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::DeleteModuleInstance(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* moduleName,
    ::CORBA::Long module_id
)
{
    //boost::ignore_unused_variable_warning( _tao_rh );
    boost::ignore_unused_variable_warning( moduleName );
    boost::ignore_unused_variable_warning( module_id );
    _tao_rh->DeleteModuleInstance();
    // Add your implementation here
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::SetParams(
    Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
    const char* moduleName,
    ::CORBA::Long module_id,
    const char* param
)
{
    boost::ignore_unused_variable_warning( moduleName );
    boost::ignore_unused_variable_warning( module_id );
    ///We are going to abuse this function for the moment to try out sending
    ///data asynchronously to condcutor and xplorer from the units
    ///to try dynamic data passing to xplorer and conductor.

    ///AMI call
    for( std::map<std::string, Body::UI_var>::iterator
            iter = m_uiMap.begin(); iter != m_uiMap.end(); )
    {
        try
        {
            iter->second->_non_existent();
            //iter->second->SetCommand( param );
            //iter->second->sendc_SetCommand( uiComAMIHandler.in(), param );

            // The callback handler servant instance holds on to a reference to the
            // AMH response handler. That way, it can forward the reply back to the
            // originial client after getting the reply from the inner server.
            PortableServer::ServantBase_var servant =
                new Body_AMI_UIHandler_i( m_poa.in(), _tao_rh );
            PortableServer::ObjectId_var objid =
                m_poa->activate_object( servant.in() );
            CORBA::Object_var obj = m_poa->id_to_reference( objid.in() );

            ::Body::AMI_UIHandler_var cb = ::Body::AMI_UIHandler::_narrow( obj.in() );

            // forward the request on to the inner server, with the callback handler
            // reference.
            iter->second->sendc_SetCommand( cb.in(), param );

            // nothing else to do. Our client will block until the callback handler
            // forwards the reply.

            ++iter;
        }
        catch( CORBA::Exception&  ex )
        {
            std::cout << "VE-CE::SetParams : " << iter->first
                      << " is obsolete." << std::endl
                      << ex._info().c_str() << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            m_uiMap.erase( iter++ );
        }
        catch( std::exception& ex )
        {
            std::cout << "VE-CE::SetParams : another kind of exception "
                      << std::endl << ex.what() << std::endl;
            ++iter;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::ClientMessage( const char* msg )
{
    std::cout << "VE-CE : Output = " << msg;
    for( std::map<std::string, Body::UI_var>::iterator
            iter = m_uiMap.begin(); iter != m_uiMap.end(); )
    {
        std::cout << "VE-CE : " << msg << " to -> " << iter->first << std::endl;
        try
        {
            iter->second->_non_existent();
            iter->second->Raise( msg );
            ++iter;
        }
        catch( CORBA::Exception& ex )
        {
            std::cout << "VE-CE : " << iter->first
                      << " is obsolete." << std::endl
                      << ex._info().c_str() << std::endl;
            // it seems this call should be blocked as we are messing with
            // a map that is used everywhere
            m_uiMap.erase( iter++ );
        }
        catch( std::exception& ex )
        {
            std::cout << "VE-CE : another kind of exception "
                      << std::endl << ex.what() << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
std::string Body_AMH_Executive_i::GetResults( int rt )
{
    //This onyl works for serial exectuion and for units with 1 input and output port
    Module* nextModule = m_network->GetModule( rt );
    if( m_modUnits.find( nextModule->GetModuleName() ) ==
            m_modUnits.end() )
    {
        std::cerr <<  "VE-CE : Cannot find running unit "
                  << nextModule->GetModuleName() << std::endl
                  << "The units that are registerd are " << std::endl;
        for( std::map< std::string, Body::Unit_var >::const_iterator iter =
                    m_modUnits.begin(); iter != m_modUnits.end(); ++iter )
        {
            std::cout << "VE-CE : Unit name = " << iter->first << std::endl;
        }
        return "return";
    }

    XMLReaderWriter networkWriter;
    std::vector< std::pair< XMLObjectPtr, std::string > > previousModuleResultsCommands;
    CommandPtr modelResults( new Command() );
    modelResults->SetCommandName( "Upstream Model Results" );
    // Now, look upstream for results data...
    unsigned int numInputPorts = nextModule->numIPorts();
    for( size_t i = 0; i < numInputPorts; ++i )
    {
        IPort* iport = nextModule->getIPort( i );
        unsigned int numConnections = iport->nconnections();
        for( size_t j = 0; j < numConnections; ++j )
        {
            Connection* conn = iport->connection( j );
            OPort* oport = conn->get_oport();
            Module* m = oport->get_module();
            std::cout << "VE-CE : Upstream Module Name: " << m->GetModuleName()
                      << " and ID " << m->get_id() << std::endl;

            {
                std::string unitResultsData = "NULL";
                //THis code needs to get ALL upstream modules for this particular module.
                //We can do this through the module interface with the port class in ce
                //call query on previous module with "Get XML Model Results"
                CommandPtr resultsCommand( new Command() );
                resultsCommand->SetCommandName( "Get XML Model Results" );
                //Get module id and unit name
                //gets tagged as vendorUnit (module name) and moduleId (number id not uuid)
                DataValuePairPtr vendorData( new DataValuePair() );
                vendorData->SetData( "vendorUnit", m->GetModuleName() );
                resultsCommand->AddDataValuePair( vendorData );
                DataValuePairPtr moduleIdData( new DataValuePair() );
                moduleIdData->SetData( "moduleId", m->get_id() );
                resultsCommand->AddDataValuePair( moduleIdData );

                //Then parse command
                std::vector< std::pair< XMLObjectPtr, std::string > > nodes;
                std::string upstreamResultsData( "returnString" );
                nodes.push_back( std::pair< CommandPtr, std::string  >(
                                     resultsCommand, std::string( "vecommand" ) ) );

                networkWriter.UseStandaloneDOMDocumentManager();
                networkWriter.WriteXMLDocument( nodes, upstreamResultsData, "Command" );
                //Now query the unit for data

                {
                    std::map<std::string, Body::Unit_var>::const_iterator
                    iter = m_modUnits.find( m->GetModuleName() );

                    if( iter == m_modUnits.end() )
                    {
                        std::cout << "VE-CE : No units to query" << std::endl;
                    }

                    try
                    {
                        iter->second->_non_existent();
                        iter->second->SetCurID( m->get_id() );
                        unitResultsData =
                            iter->second->Query(
                                CORBA::string_dup( upstreamResultsData.c_str() ) );
                    }
                    catch( CORBA::Exception& ex )
                    {
                        std::cout << "VE-CE : Module Query Messed up." << std::endl;
                        std::cerr << "VE-CE : CORBA exception raised: " << ex._name() << std::endl;
                        std::cerr << ex._info().c_str() << std::endl;
                    }
                }

                if( unitResultsData != "NULL" )
                {
                    XMLReaderWriter networkReader;
                    networkReader.UseStandaloneDOMDocumentManager();
                    networkReader.ReadFromString();
                    networkReader.ReadXMLData( unitResultsData, "Command", "vecommand" );
                    std::vector< XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
                    CommandPtr tempResult =  boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
                    size_t numDVPResults = tempResult->GetNumberOfDataValuePairs();
                    for( size_t k = 0; k < numDVPResults; ++k )
                    {
                        DataValuePairPtr tempDVP = tempResult->GetDataValuePair( k );
                        modelResults->AddDataValuePair( tempDVP );
                    }
                }
            }
        }
    }
    //Then set inputs on next module
    previousModuleResultsCommands.push_back(
        std::pair< CommandPtr, std::string  >( modelResults, std::string( "vecommand" ) ) );


    //Now get the input data for the current model
    const std::vector< CommandPtr > inputList =
        nextModule->GetVEModel()->GetInputs();
    for( size_t k = 0; k < inputList.size(); ++k )
    {
        previousModuleResultsCommands.push_back(
            std::pair< CommandPtr, std::string  >( inputList.at( k ), std::string( "vecommand" ) ) );
    }
    std::string resultsData;
    resultsData.assign( "returnString" );
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.WriteXMLDocument( previousModuleResultsCommands, resultsData, "Command" );

    return resultsData;
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::execute_next_mod( long module_id )
{
    ///This is the 0 based number of the index for the module_id
    int moduleIndex = m_network->moduleIdx( module_id );
    if( moduleIndex < 0 )
    {
        std::cerr << "Unit ID " << module_id
                  << " cannot be found in the list of available units." << std::endl;
        return;
    }

    //module_id is the currently active module_id and this function
    //is determining which is the next module to execute
    //id is module jsut executed
    std::string msg;

    std::string mod_type = m_network->GetModule( moduleIndex )->GetModuleName();
    if( m_modUnits.find( mod_type ) != m_modUnits.end() )
    {
        try
        {
            m_modUnits[ mod_type ]->_non_existent();
            msg = m_modUnits[ mod_type ]->GetStatusMessage();
        }
        catch( CORBA::Exception& )
        {
            std::cerr << "Cannot contact module id " << module_id
                      << " name " << mod_type << std::endl;
            return;
        }
    }
    else
    {
        std::cerr << "Module name " << mod_type
                  << " is not yet connected." << std::endl;
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

    CommandPtr returnState =
        boost::dynamic_pointer_cast<ves::open::xml::Command>(
            objectVector.at( 0 ) );

    long rs;
    // 0:O.K, 1:ERROR, 2:?, 3:FB COMLETE
    returnState->GetDataValuePair( "RETURN_STATE" )->GetData( rs );

    if( rs == -1 )
    {
        returnState->GetDataValuePair( "return_state" )->GetData( rs );
    }
    //delete the object vector
    objectVector.clear();

    m_network->GetModule( moduleIndex )->_return_state = rs;
    //If we have an error - see above
    if( rs == 1 )
    {
        return;
    }

    //rt is the module to be executed next
    //rt goes from 0 = no module to execute to
    //1 -> n where 1 is the first module executed and so on
    //rt is the next module index and -1 is subtracted to make the number 0 based
    //to be able to index into the vector of modules stored in the network class
    int rt = m_scheduler->execute( m_network->GetModule( moduleIndex ) ) - 1;
    std::cout << "VE-CE::execute_next_mod Vector id of next module to execute "
              << rt << " Just executed module ID " << module_id << std::endl;
    if( rt < 0 )
    {
        ClientMessage( "VE-Suite Network Execution Complete\n" );
        return;
    }

    const std::string resultsData = GetResults( rt );

    try
    {
        unsigned int tempID = m_network->GetModule( rt )->get_id();
        m_modUnits[ m_network->GetModule( rt )->GetModuleName() ]->
        SetCurID( tempID );
        if( resultsData != "NULL" )
        {
            m_modUnits[ m_network->GetModule( rt )->GetModuleName()]->
            SetParams( tempID, resultsData.c_str() );
        }
        execute( m_network->GetModule( rt )->GetModuleName() );
    }
    catch( CORBA::Exception& ex )
    {
        std::cerr << "VE-CE : Cannot contact Module " << module_id
                  << std::endl << ex._info().c_str() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Body_AMH_Executive_i::execute( std::string mn )
{
    std::string msg;

    if( m_execThread.find( mn ) == m_execThread.end() )
    {
        std::cerr << "Cannot find execution thread for " << mn << std::endl;
    }
    else
    {
        if( !m_execThread[mn]->needexecute() )
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
