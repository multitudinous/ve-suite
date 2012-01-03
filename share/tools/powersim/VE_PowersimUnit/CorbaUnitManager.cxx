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

// --- VE_PowersimUnit Includes --- //
#include "StdAtl.h"
#include "Resource.h"
#include "CorbaUnitManager.h"
#include "VE_PowersimUnit_i.h"
#include "MainDlg.h"
//#include "SIPParser.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>

// --- XERCESC Includes --- //
#include <xercesc/dom/DOM.hpp>

// --- TAO Includes --- //
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <orbsvcs/CosNamingC.h>

// --- C/C++ Includes  -- //
#include <fstream>
#include <iostream>

XERCES_CPP_NAMESPACE_USE

////////////////////////////////////////////////////////////////////////////////
CorbaUnitManager::CorbaUnitManager( CMainDlg* mainDialog )
    :
    parent( mainDialog ),
    unit_i( 0 ),
    unit_i_instantiated( false )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CorbaUnitManager::~CorbaUnitManager()
{
    XMLPlatformUtils::Terminate();
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::SetComputerNameUnitNameAndPort(
    const ATL::CString& workingDir,
    const ATL::CString& computerName,
    const ATL::CString& computerPort,
    const ATL::CString& unitName )
{
    m_workingDir = workingDir;
    m_computerName = computerName;
    m_computerPort = computerPort;
    m_unitName = unitName;
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::SetRunORBFlag( bool run )
{
    //runORB = run;
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::RunORB()
{
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch( const XMLException& toCatch )
    {
        XERCES_STD_QUALIFIER cerr
            << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode( toCatch.getMessage() )
            << XERCES_STD_QUALIFIER endl;

        exit( 1 );
    }

    //Initialize VE-Open
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "XML", new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "Shader", new ves::open::xml::shader::ShaderCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "Model", new ves::open::xml::model::ModelCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator(
        "CAD", new ves::open::xml::cad::CADCreator() );

    int argc = 5;
    char** argv;
    std::vector< char* > cmdargs;

    cmdargs.push_back( "VE_AspenUnit.exe" );
    cmdargs.push_back( "-ORBInitRef" );
    std::string orbInfo = std::string(
        "NameService=corbaloc:iiop:" + m_computerName +
        ":" + m_computerPort + "/NameService" );

    cmdargs.push_back( const_cast< char* >( orbInfo.c_str() ) );
    cmdargs.push_back( "-ORBDottedDecimalAddresses" );
    cmdargs.push_back( "1" );

    argv = new char*[ argc ];
    for( unsigned int i = 0; i < argc; ++i )
    {
        argv[ i ] = new char [ strlen( cmdargs.at( i ) ) + 1 ];
        strcpy( argv[ i ], cmdargs.at( i ) );
    }

    std::string UNITNAME = "POWERSIMUNIT";
    std::cout << "Unit name is:" << m_unitName << std::endl;

    try 
    {
        // First initialize the ORB, 
        orb = CORBA::ORB_init(
            argc, argv, "" /* the ORB name, it can be anything! */ );

        //Contact the naming service and get the reference for the executive
        CORBA::Object_var naming_context_object =
            orb->resolve_initial_references( "NameService" );
        CosNaming::NamingContext_var naming_context =
            CosNaming::NamingContext::_narrow( naming_context_object.in() );

        CosNaming::Name name( 1 );
        name.length( 1 );
        name[ 0 ].id = CORBA::string_dup( "Executive" );

        CORBA::Object_var exec_object = naming_context->resolve( name );

        //Now downcast the object reference to the appropriate type
        exec = Body::Executive::_narrow( exec_object.in() );

        //Here is the code to set up the server
        CORBA::Object_var poa_object =
            orb->resolve_initial_references( "RootPOA" );
        poa = PortableServer::POA::_narrow( poa_object.in() );
        PortableServer::POAManager_var poa_manager = poa->the_POAManager();
        CORBA::PolicyList policies( 1 );
        policies.length( 1 );
        CORBA::Any pol;

        pol <<= BiDirPolicy::BOTH;
        policies[ 0 ] = orb->create_policy(
            BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE, pol ACE_ENV_ARG_PARAMETER );
        ACE_TRY_CHECK;

        //Create POA as child of RootPOA with the above policies
        //This POA will receive request in the same connection in which it sent the request
        PortableServer::POA_var child_poa = poa->create_POA(
            "childPOA", poa_manager.in(), policies ACE_ENV_ARG_PARAMETER );
        ACE_TRY_CHECK;

        //Creation of childPOA is over; Destroy the Policy objects.
        for( CORBA::ULong i = 0; i < policies.length (); ++i )
        {
            policies[ i ]->destroy( ACE_ENV_SINGLE_ARG_PARAMETER );
            ACE_TRY_CHECK;
        }
        poa_manager->activate( ACE_ENV_SINGLE_ARG_PARAMETER );
        ACE_TRY_CHECK;

        //Create the Servant
        unit_i = new Body_Unit_i(
            UNITNAME, parent, this, std::string( m_workingDir ) );
        unit_i_instantiated = true;

        //Activate it to obtain the object reference
        PortableServer::ObjectId_var id =
            PortableServer::string_to_ObjectId(
                CORBA::string_dup( UNITNAME.c_str() ) );
        child_poa->activate_object_with_id(
            id.in(), unit_i ACE_ENV_ARG_PARAMETER );

        //Activate it to obtain the object reference
        Body::Unit_var unit = Body::Unit::_narrow(
            child_poa->id_to_reference(
                id.in() ACE_ENV_ARG_PARAMETER ) );
        ACE_TRY_CHECK;

        CosNaming::Name Unitname( 1 );
        Unitname.length( 1 );
        Unitname[ 0 ].id = CORBA::string_dup( UNITNAME.c_str() );
        //Bind the object
        try
        {
            naming_context->bind( Unitname, unit.in() );
        }
        catch( CosNaming::NamingContext::AlreadyBound& ex )
        {
            naming_context->rebind( Unitname, unit.in() );
            std::cout << ex._info().c_str() << std::endl;
        }

        //Call the Executive CORBA call to register it to the Executive
        exec->RegisterUnit(
            unit_i->UnitName_.c_str(), unit.in(), 0 /*0 is a normal module*/ );
    }
    catch( CORBA::Exception& )
    {
        std::cerr << "CORBA exception raised!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::DestroyORB()
{	  
    if( !unit_i_instantiated )
    {
        return;
    }

    //unit_i->ClosePowersim();
    //Sleep( 5000 );

    CleanUp();
}
////////////////////////////////////////////////////////////////////////////////
Body_Unit_i* const CorbaUnitManager::GetUnitObject() const
{
   return unit_i;
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::CheckCORBAWork()
{
    if( !CORBA::is_nil( orb ) )
    {
        if( orb->work_pending() )
        {
            orb->perform_work();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
/*
SIPParser* CorbaUnitManager::CreateParser()
{
    return new SIPParser();
}
*/
////////////////////////////////////////////////////////////////////////////////
bool CorbaUnitManager::CleanUp()
{
    try
    {
        exec->UnRegisterUnit( unit_i->UnitName_.c_str() );
        delete unit_i;
        unit_i = NULL;

    }
    catch ( CORBA::SystemException& ex )
    {
        return false;
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
