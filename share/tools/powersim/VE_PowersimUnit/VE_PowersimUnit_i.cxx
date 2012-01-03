/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

    m_powersimLog = m_mainDialog->GetDlgItem( IDC_LOG );

    m_queryCommandNames.insert( "GetNetwork" );
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

    if( cmdname == "GetNetwork" )
    {
        returnValue = HandleGetNetwork( cmd );

        return returnValue;
    }
    else if( cmdname == "OpenSimulation" )
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
char* Body_Unit_i::HandleGetNetwork( ves::open::xml::CommandPtr cmd )
{
    std::string fileName = cmd->GetDataValuePair( 1 )->GetDataString();
    std::string extension = fileName.substr( fileName.size() - 4, 4 );
    if( extension.find( "sip" ) != std::string::npos )
    {
        fileName.resize( fileName.size() - 4 );
        m_sipParser = new SIPParser();
        m_sipParser->SetWorkingDir( m_workingDir );
    }

    //This command has no params
    //bool firsttime = true;
    //if( firsttime )
    //{
        //Make sure sip file exists
        std::ifstream sipFile(
            ( m_workingDir + fileName + ".sip" ).c_str(), std::ios::binary );
        if( !sipFile.is_open() )
        {
            //No sip file
            LPCTSTR logText( "SIP File Does NOT exist.\r\n" );
            m_mainDialog->SendMessage( m_powersimLog, EM_SETSEL, -1, -1 );
            m_mainDialog->SendMessage(
                m_powersimLog, EM_REPLACESEL, 0,
                reinterpret_cast< LPARAM >( logText ) );

            return CORBA::string_dup( "SIPDNE" );
        }
        sipFile.close();

        m_mainDialog->SetDlgItemText( IDC_FILENAME, fileName.c_str() );

        //Go through sip parsing procedure
        //m_sipParser->OpenSimAndParse( fileName.c_str() );
        m_fileName = fileName;

        //firsttime = false;
    //}

    std::string network;
    try
    {
        //network = m_sipParser->CreateNetwork();
    }
    catch( ... )
    {
        std::cout << "GetNetwork Exception Powersim Unit" << std::endl;
        return NULL;
    }

    return CORBA::string_dup( network.c_str() );
}
////////////////////////////////////////////////////////////////////////////////
char* Body_Unit_i::HandleOpenSimulation( ves::open::xml::CommandPtr cmd )
{
    //This command has no params
    std::string fileName = cmd->GetDataValuePair( 1 )->GetDataString();

    std::string extension = fileName.substr( fileName.size() - 4, 4 );
    if( extension.find( "sip" ) != std::string::npos )
    {
        fileName.resize( fileName.size() - 4 );
        //Make sure sip file exists
        std::ifstream sipFile(
            ( m_workingDir + fileName + ".sip" ).c_str(), std::ios::binary );
        if( !sipFile.is_open() )
        {
            //No sip file
            LPCTSTR logText( "SIP File Does NOT exist.\r\n" );
            m_mainDialog->SendMessage( m_powersimLog, EM_SETSEL, -1, -1 );
            m_mainDialog->SendMessage(
                m_powersimLog, EM_REPLACESEL, 0,
                reinterpret_cast< LPARAM >( logText ) );

            return CORBA::string_dup( "SIPDNE" );
        }
        sipFile.close();

        m_mainDialog->SetDlgItemText( IDC_FILENAME, fileName.c_str() );
        m_sipParser = new SIPParser();
        m_sipParser->SetWorkingDir( m_workingDir );
        m_sipParser->OpenSimulation( fileName );
    }

    return CORBA::string_dup( "Simulation Opened." );
}
////////////////////////////////////////////////////////////////////////////////