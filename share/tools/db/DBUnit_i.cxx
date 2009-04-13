
// --- My Includes --- //
#include "DBUnit_i.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Command.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <sstream>

////////////////////////////////////////////////////////////////////////////////
Body_Unit_i::Body_Unit_i( Body::Executive_ptr exec, std::string name )
:
executive_( Body::Executive::_duplicate( exec ) )
{
    UnitName_ = name;
    return_state = 0;
}
////////////////////////////////////////////////////////////////////////////////
Body_Unit_i::~Body_Unit_i()
{
    ;
}
//////////////////////////////////////////////////////////////////////////////// 
void Body_Unit_i::StartCalc()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::cout << UnitName_ << " :Start Calc called" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::StopCalc()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::string msg;
    msg = UnitName_ + " :Instant calculation, already finished\n";
    executive_->SetModuleMessage( id_, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::PauseCalc()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::string msg;
    msg = UnitName_ + " :Instant calculation, already finished\n";
    executive_->SetModuleMessage( id_, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::Resume()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::string msg;
    msg = UnitName_ + " :Instant calculation, already finished\n";
    executive_->SetModuleMessage( id_, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::GetStatusMessage()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here
    std::cout << UnitName_ << " :GetStatusMessage called" << std::endl;

    return NULL;//CORBA::string_dup( status );
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::GetUserData()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::cout << UnitName_ << " :GetUserData called" << std::endl;

    return NULL;//CORBA::string_dup( data_.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetParams( CORBA::Long id, const char* param )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    if( std::string( param ) == "" )
    {
        return;
    }
    std::cout << UnitName_ << " :SetParams called" << std::endl;

    /*
    VE_XML::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( param, "Command", "vecommand" );
    std::vector< VE_XML::XMLObject* >objectVector = networkWriter.GetLoadedXMLObjects();

    std::cout << "Object vector size:" << objectVector.size() << std::endl;
    std::cout << param << std::endl;

    for( size_t i = 0; i < objectVector.size(); ++i )
    {
        VE_XML::Command* params = dynamic_cast< VE_XML::Command* >( objectVector.at( i ) );
        //unsigned int num = ( unsigned int )params->GetNumberOfDataValuePairs();

        VE_XML::DataValuePair* curPair;
        if( params->GetCommandName() == "sensorData" )
        {
            curPair = params->GetDataValuePair( "sensorData" );
            curPair->GetData( sensorData );
        }

        //CASI::Variable cur_var = bkp.aspendoc.getVarByNodePath( nodepath );
        //cur_var.setValue( nodevalue );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetID( CORBA::Long id )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    id_ = id;
    std::cout << UnitName_ << " :SetID called" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
Types::ArrayLong* Body_Unit_i::GetID()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::cout << UnitName_ << " :GetID called" << std::endl;

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetName( const char* name )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    UnitName_ = std::string( name );
    std::cout << UnitName_ << " :SetName called" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::GetName()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::cout << UnitName_ << " :GetName called" << std::endl;

    return CORBA::string_dup( UnitName_.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::Query( const char* command )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::string network;
    network = "NULL";
    /*
    std::cout << "UnitWrapper::Query called = " << command << std::endl;
    VE_XML::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( command, "Command", "vecommand" );
    std::vector< VE_XML::XMLObject* > objectVector = networkWriter.GetLoadedXMLObjects();

    std::string network;
    //The query function assumes 1 command to be processed at a time
    if( objectVector.size() > 1 )
    {
        network.assign( "Must send 1 command at a time" );
        return CORBA::string_dup( network.c_str() );
    }

    std::ostringstream strm;
    strm << activeId;

    VE_XML::Command* params = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );
    std::string commandName = params->GetCommandName();
    std::map< std::string, VE_CE::EventHandler* >::iterator currentEventHandler;
    currentEventHandler = eventHandlerMap.find( commandName );
    if( currentEventHandler != eventHandlerMap.end() )
    {
        currentEventHandler->second->SetBaseObject( xmlModelMap[ strm.str() ] );
        network = currentEventHandler->second->Execute( objectVector );
    }
    */

    return CORBA::string_dup( network.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetCurID( CORBA::Long id )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

}
////////////////////////////////////////////////////////////////////////////////
::CORBA::Long Body_Unit_i::GetCurID()
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::DeleteModuleInstance( CORBA::Long module_id )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::error( std::string msg )
{
    //Add your implementation here

}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::warning( std::string msg )
{
    //Add your implementation here

    msg += "\n";
    executive_->SetModuleMessage( id_, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
