/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/ce/unitwrapper/UnitWrapper.h>

#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/Command.h>

#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>

// Event handlers
#include <ves/ce/unitwrapper/SetInputsEventHandler.h>
#include <ves/ce/unitwrapper/GetInputsEventHandler.h>
//#include <ves/ce/unitwrapper/SetResultsEventHandler.h>
#include <ves/ce/unitwrapper/GetResultsEventHandler.h>
#include <ves/ce/unitwrapper/EventHandler.h>

#include <sstream>
////////////////////////////////////////////////////////////////////////////////
UnitWrapper::UnitWrapper( Body::Executive_ptr exec, std::string name )
    : 
    executive_( Body::Executive::_duplicate( exec ) ),
    UnitName_( name ),
    return_state( 0 )
{
    ///Initialize VE-Open
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "XML", new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "Shader", new ves::open::xml::shader::ShaderCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "Model", new ves::open::xml::model::ModelCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->
        RegisterObjectCreator( "CAD", new ves::open::xml::cad::CADCreator() );

    eventHandlerMap[ "Set XML Model Inputs" ] = new VE_CE::SetInputsEventHandler();
    eventHandlerMap[ "Get XML Model Inputs" ] = new VE_CE::GetInputsEventHandler();
    //eventHandlerMap[ "Set XML Model Results" ] = new VE_CE::SetResultsEventHandler();
    eventHandlerMap[ "Get XML Model Results" ] = new VE_CE::GetResultsEventHandler();
    //eventHandlerMap[ "Get XML Model Port Data" ] = new VE_CE::SetInputsEventHandler();
    //eventHandlerMap[ "Set XML Model Port Data" ] = new VE_CE::SetInputsEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
///Default constructor
////////////////////////////////////////////////////////////////////////////////
UnitWrapper::UnitWrapper()
{
}
////////////////////////////////////////////////////////////////////////////////
UnitWrapper::~UnitWrapper( void )
{
    for( std::map< std::string, VE_CE::EventHandler* >::iterator 
        currentEventHandler = eventHandlerMap.begin(); 
        currentEventHandler != eventHandlerMap.end();
        ++currentEventHandler )
    {
        delete currentEventHandler->second;
    }
    
    eventHandlerMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::StartCalc()
{
    std::string msg;
    msg = UnitName_ + " :Start Calc called\n";
    executive_->SetModuleMessage( activeId, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::StopCalc()
{
    std::string msg;
    msg = UnitName_ + " : StopCalc\n";
    executive_->SetModuleMessage( activeId, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::PauseCalc()
{
    std::string msg;
    msg = UnitName_ + " : PauseCalc\n";
    executive_->SetModuleMessage( activeId, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::Resume()
{
    std::string msg;
    msg = UnitName_ + " : Resume\n";
    executive_->SetModuleMessage( activeId, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetStatusMessage()
{
    // Add your implementation here
    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );

    returnState->SetCommandName( "statusmessage" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetDataName( "RETURN_STATE" );
    data->SetDataType( "UNSIGNED INT" );
    data->SetDataValue( return_state );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair < ves::open::xml::XMLObjectPtr,
                     std::string > ( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );
    return CORBA::string_dup( status.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetUserData()
{
    // Add your implementation here
    char * result = 0;
    return result;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetParams( ::CORBA::Long id, const char * param )
{
    //just send a list command to SetParams
    // the eventhandler will handle the rest
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( param, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();
    std::ostringstream idString;
    idString << id;
    eventHandlerMap[ "Set XML Model Inputs" ]->SetBaseObject( xmlModelMap[ idString.str()] );
    if( !objectVector.empty() )
    {
        eventHandlerMap[ "Set XML Model Inputs" ]->Execute( objectVector );
    }
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetID(::CORBA::Long id)
{
    std::ostringstream strm;
    strm << id;

    std::map< std::string, ves::open::xml::model::ModelPtr >::iterator iter;
    iter = xmlModelMap.find( strm.str() );
    if( iter == xmlModelMap.end() )
    {
        xmlModelMap[ strm.str()] = ves::open::xml::model::ModelPtr( new ves::open::xml::model::Model() );
    }

    std::cout << UnitName_ << " :SetID called" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetCurID( ::CORBA::Long id )
{
    activeId = id;
    std::cout << UnitName_ << " :SetCurID called" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
::Types::ArrayLong* UnitWrapper::GetID()
{
    std::cout << UnitName_ << " :GetID called" << std::endl;
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
::CORBA::Long UnitWrapper::GetCurID()
{
    // This function returns the id of the currently executed module
    return activeId;
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::SetName( const char * name )
{
    // Add your implementation here
    UnitName_ = std::string( name );
    std::cout << UnitName_ << " :SetName called" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::GetName()
{
    std::cout << UnitName_ << " :GetName called" << std::endl;
    return CORBA::string_dup( UnitName_.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
char * UnitWrapper::Query( const char* command )
{
    //use print statement below to check command coming in
    std::cout << "UnitWrapper::Query called = " << command << std::endl;
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( command, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkWriter.GetLoadedXMLObjects();

    std::string network;
    //The query function assumes 1 command to be processed at a time
    if( objectVector.size() > 1 )
    {
        network.assign( "NULL" );
        return CORBA::string_dup( network.c_str() );
    }

    std::ostringstream strm;
    strm << activeId;

    ves::open::xml::CommandPtr params = boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
    std::string commandName = params->GetCommandName();
    std::map< std::string, VE_CE::EventHandler* >::iterator currentEventHandler;
    currentEventHandler = eventHandlerMap.find( commandName );
    if( currentEventHandler != eventHandlerMap.end() )
    {
        currentEventHandler->second->SetBaseObject( xmlModelMap[ strm.str()] );
        network = currentEventHandler->second->Execute( objectVector );
    }

    if( network.empty() )
    {
        network = "NULL";
    }
    //use print statement below to check network string
    std::cout << "UnitWrapper::Query result " << network << std::endl;
    return CORBA::string_dup( network.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void UnitWrapper::DeleteModuleInstance( ::CORBA::Long module_id )
{
    std::ostringstream strm;
    strm << module_id;

    std::map< std::string, ves::open::xml::model::ModelPtr >::iterator iter;
    iter = xmlModelMap.find( strm.str() );
    if( iter != xmlModelMap.end() )
    {
        xmlModelMap.erase( iter );
    }
}
////////////////////////////////////////////////////////////////////////////////
