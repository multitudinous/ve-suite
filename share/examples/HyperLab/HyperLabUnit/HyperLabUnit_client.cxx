// --- My Includes --- //
#include "HyperLabUnit_i.h"
#include "HyperLabUnit_client.h"

// --- ACE-TAO Includes --- //
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <orbsvcs/CosNamingC.h>

// --- VE-Suite Includes --- //
#include <ves/open/moduleC.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <vector>

XERCES_CPP_NAMESPACE_USE

////////////////////////////////////////////////////////////////////////////////
CorbaUnitManager::CorbaUnitManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CorbaUnitManager::~CorbaUnitManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
/*
BOOLCorbaUnitManager::InitInstance()
{
    runORB = false;
    unit_i = 0;

    while( 1 )
    {
        Sleep( 10 );

        if( runORB )
        {
            RunORB();
        }
    }

    return FALSE;
}
*/
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::SetRunORBFlag( bool run )
{
    ;//runORB = run;
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::RunORB( std::string computerName, std::string portNumber, std::string unitName )
{
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch( const XMLException &toCatch )
    {
        XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
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

    int argc = 7;
    char** argv;
    std::vector< char* > cmdargs;

    cmdargs.push_back( "HyperLabUnit.exe" );
    cmdargs.push_back( "-ORBInitRef" );
    std::string orbInfo = std::string( "NameService=corbaloc:htiop:" ) +
                          computerName +
                          std::string( ":" ) + 
                          portNumber +
                          std::string( "/NameService" );
    cmdargs.push_back( const_cast< char* >( orbInfo.c_str() ) );
    cmdargs.push_back( "-ORBSvcConf" );
    cmdargs.push_back( "inside.conf" );
    cmdargs.push_back( "-ORBDottedDecimalAddresses" );
    cmdargs.push_back( "1" );

    argv = new char*[ argc ];

    for( int i = 0; i < argc; ++i )
    {
        argv[ i ] = new char[ strlen( cmdargs.at( i ) ) + 1 ];
        strcpy( argv[ i ], cmdargs.at( i ) );
    }

    //Unit name is "HyperLab"
    std::string UNITNAME = unitName;
    std::cout << "Unit name is :" << unitName << std::endl;

    try
    {
        //First initialize the ORB, 
        orb = CORBA::ORB_init( argc, argv, "" /* the ORB name, it can be anything! */ );

        //Here is the part to contact the naming service and get the reference for the executive
        CORBA::Object_var naming_context_object = orb->resolve_initial_references( "NameService" );
        CosNaming::NamingContext_var naming_context = CosNaming::NamingContext::_narrow( naming_context_object.in() );

        CosNaming::Name name( 1 );
        name.length( 1 );
        name[ 0 ].id = CORBA::string_dup( "Executive" );

        CORBA::Object_var exec_object = naming_context->resolve( name );

        //Now downcast the object reference to the appropriate type
        Body::Executive_var exec = Body::Executive::_narrow( exec_object.in() );

        // other client code

        //...........

        //Here is the code to set up the server
        //Get the root poa
        CORBA::Object_var poa_object = orb->resolve_initial_references( "RootPOA" );
        PortableServer::POA_var poa = PortableServer::POA::_narrow( poa_object.in() );
        PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
        poa_manager->activate();

        //Create the Servant
        unit_i = new Body_Unit_i( exec.in(), UNITNAME );

        //Activate it to obtain the object reference
        Body::Unit_var unit = unit_i->_this();

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
        }

        //Call the Executive CORBA call to register it to the Executive
        std::cout << unit_i->UnitName_.c_str() << std::endl;
        exec->RegisterUnit( unit_i->UnitName_.c_str(), unit.in(), 0 ); //0 means a normal module

        //dlgThread->dlg.SetVESUnit( &unit_i );
        //orb->run();
        //Destroy the POA, waiting until the destruction terminates
        //poa->destroy( 1, 1 );
        //Finally destroy the ORB
        //orb->destroy();
    }
    catch( CORBA::Exception & )
    {
        std::cerr << "CORBA exception raised!" << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
Body_Unit_i* CorbaUnitManager::GetUnitObject()
{
    return unit_i;
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::CheckCORBAWork()
{
    if( !CORBA::is_nil( orb ) )
    {
        if(orb->work_pending())
        {
            orb->perform_work();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
