
// --- VE_Powersim Includes --- //
#include "StdAtl.h"
#include "Resource.h"
#include "VE_PowersimUnit_i.h"
#include "MainDlg.h"
#include "CorbaUnitManager.h"
#include "SIPParser.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Port.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>

// --- C/C++ Includes --- //
#include <fstream>
#include <iostream>

namespace vox = ves::open::xml;

////////////////////////////////////////////////////////////////////////////////
Body_Unit_i::Body_Unit_i(
    const std::string& unitName,
    CMainDlg* mainDialog,
    CorbaUnitManager* corbaUnitManager,
    const std::string& workingDir )
    :
    m_returnState( 0 ),
    m_mainDialog( mainDialog ),
    m_corbaUnitManager( corbaUnitManager ),
    m_workingDir( workingDir )
{
    vox::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "XML", new vox::XMLCreator() );
    vox::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "Shader", new vox::shader::ShaderCreator() );
    vox::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "Model", new vox::model::ModelCreator() );
    vox::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "CAD", new vox::cad::CADCreator() );

    m_queryCommandNames.insert( "OpenSimulation" );

}
////////////////////////////////////////////////////////////////////////////////
Body_Unit_i::~Body_Unit_i()
{
    if( m_sipParser )
    {
        delete m_sipParser;
    }
}
//////////////////////////////////////////////////////////////////////////////// 
void Body_Unit_i::StartCalc( ACE_ENV_SINGLE_ARG_DECL )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::cout << UnitName_ << " :Start Calc called" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::StopCalc( ACE_ENV_SINGLE_ARG_DECL )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::string msg;
    msg = UnitName_ + " :Instant calculation, already finished\n";
    //executive_->SetModuleMessage( id_, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::PauseCalc( ACE_ENV_SINGLE_ARG_DECL )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::string msg;
    msg = UnitName_ + " :Instant calculation, already finished\n";
    //executive_->SetModuleMessage( id_, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::Resume( ACE_ENV_SINGLE_ARG_DECL )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    std::string msg;
    msg = UnitName_ + " :Instant calculation, already finished\n";
    //executive_->SetModuleMessage( id_, msg.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::GetStatusMessage( ACE_ENV_SINGLE_ARG_DECL )
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
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetID( CORBA::Long id )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

    //No need to implement this
    std::cout << "This is not implemented for the Powersim Unit" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
Types::ArrayLong* Body_Unit_i::GetID( ACE_ENV_SINGLE_ARG_DECL )
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
char* Body_Unit_i::GetName( ACE_ENV_SINGLE_ARG_DECL )
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
    ves::open::xml::XMLReaderWriter networkWriter;
    networkWriter.UseStandaloneDOMDocumentManager();
    networkWriter.ReadFromString();
    networkWriter.ReadXMLData( command, "Command", "vecommand" );

    std::vector< vox::XMLObjectPtr > objectVector =
        networkWriter.GetLoadedXMLObjects();

    vox::CommandPtr cmd =
        boost::dynamic_pointer_cast< vox::Command >( objectVector.at( 0 ) );
    std::string cmdname = cmd->GetCommandName();
    
    std::set< std::string >::const_iterator commandItr =
        m_queryCommandNames.find( cmdname );

    char* returnValue = "empty"; 

    //If the command is not processed here - do not bother doing anything more
    if( commandItr == m_queryCommandNames.end() )
    {
        return CORBA::string_dup( "NULL" );
    }

    if( cmdname == "OpenSimulation" )
    {
        returnValue = HandleOpenSimulation( cmd );

        return returnValue;
    }
    else
    {
        return CORBA::string_dup( "NULL" );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Body_Unit_i::SetCurID( CORBA::Long id )
ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) )
{
    //Add your implementation here

}
////////////////////////////////////////////////////////////////////////////////
CORBA::Long Body_Unit_i::GetCurID( ACE_ENV_SINGLE_ARG_DECL )
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
char* Body_Unit_i::HandleOpenSimulation( ves::open::xml::CommandPtr cmd )
{
    CEdit fileName;
    fileName.Attach( m_mainDialog->GetDlgItem( IDC_FILENAME ) );

    //This command has no params
    std::string filename = cmd->GetDataValuePair( 1 )->GetDataString();

    std::string extension = filename.substr( filename.size() - 4, 4 );
    if( extension.find( "sip" ) != std::string::npos )
    {
        filename.resize( filename.size() - 4 );
        //Make sure sip file exists
        std::ifstream sipFile(
            ( m_workingDir + filename + ".sip" ).c_str(), std::ios::binary );
        if( !sipFile.is_open() )
        {
            //No sip file
            //AspenLog->SetSel( -1, -1 );
            //AspenLog->ReplaceSel( "SIP File Does NOT exist.\r\n" );

            return CORBA::string_dup( "SIPDNE" );
        }
        sipFile.close();

        fileName.SetWindowText( filename.c_str() );
        m_sipParser = new SIPParser();
        m_sipParser->SetWorkingDir( m_workingDir );
        m_sipParser->OpenSimulation( filename );
    }

    return CORBA::string_dup( "Simulation Opened." );
}
////////////////////////////////////////////////////////////////////////////////