#include "stdafx.h"
#include <xercesc/dom/DOM.hpp>
#include "CorbaUnitManager.h"
#include "orbsvcs/CosNamingC.h"
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include "VE_AspenUnit.h"

#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <fstream>
#include <iostream>

XERCES_CPP_NAMESPACE_USE


CorbaUnitManager::CorbaUnitManager(CVE_AspenUnitDlg * dialog)
    :
    parent( dialog ),
    unit_i( 0 ),
    unit_i_instantiated( false )
{
}
////////////////////////////////////////////////////////////////////////////////
CorbaUnitManager::~CorbaUnitManager()
{
    XMLPlatformUtils::Terminate();
}
////////////////////////////////////////////////////////////////////////////////
void CorbaUnitManager::SetComputerNameUnitNameAndPort( CString dir, CString name, CString port, CString uname )
{
   computerName = name;
   computerPort = port;
   unitName = uname;
   workingDir = dir;
}
/////////////////////////////////////////////////////////////////
void CorbaUnitManager::SetRunORBFlag( bool run )
{
   //runORB = run;
}
/////////////////////////////////////////////////////////////////
void CorbaUnitManager::RunORB()
{
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch(const XMLException &toCatch)
    {
        XERCES_STD_QUALIFIER cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode(toCatch.getMessage())
            << XERCES_STD_QUALIFIER endl;
        exit(1);
    }

    ///Initialize VE-Open
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML",new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader",new ves::open::xml::shader::ShaderCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "Model",new ves::open::xml::model::ModelCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD",new ves::open::xml::cad::CADCreator() );

    //initialize OLE
    CoInitialize(NULL);

    int argc = 5;
    char** argv;
    std::vector< char* > cmdargs;

    cmdargs.push_back( "VE_AspenUnit.exe" );
    cmdargs.push_back( "-ORBInitRef" );
    //std::string orbInfo = std::string("NameService=corbaloc:iiop:localhost:1239/NameService");
    std::string orbInfo = std::string("NameService=corbaloc:iiop:"+computerName+":"+computerPort+"/NameService");
    cmdargs.push_back( (char*)orbInfo.c_str() );
    cmdargs.push_back( "-ORBDottedDecimalAddresses" );
    cmdargs.push_back( "1" );

    //CEdit *Display;
    //Display = reinterpret_cast<CEdit *>(parent->GetDlgItem(IDC_EDIT3));
    //Display->SetWindowText("localhost");
    //Display = reinterpret_cast<CEdit *>(parent->GetDlgItem(IDC_EDIT4));
    //Display->SetWindowText("1239");

    argv = new char*[ argc ];

    for( int i=0; i<argc; i++ )
    {
        argv[ i ] = new char [ strlen( cmdargs.at( i ) ) + 1 ];
        strcpy( argv[ i ], cmdargs.at( i ) );
    }

    std::string UNITNAME = "ASPENUNIT";
    std::cout<<"Unit name is :"<<unitName<<std::endl;

    try 
    {
        // First initialize the ORB, 
        orb = CORBA::ORB_init (argc, argv, "" /* the ORB name, it can be anything! */);

        //Here is the part to contact the naming service and get the reference for the executive
        CORBA::Object_var naming_context_object =
        orb->resolve_initial_references ("NameService");
        CosNaming::NamingContext_var naming_context =
        CosNaming::NamingContext::_narrow (naming_context_object.in ());

        CosNaming::Name name(1);
        name.length(1);
        name[0].id = CORBA::string_dup ("Executive");

        CORBA::Object_var exec_object = naming_context->resolve(name);

        //Now downcast the object reference to the appropriate type
        exec = Body::Executive::_narrow(exec_object.in());

        //Here is the code to set up the server
        CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
        poa = PortableServer::POA::_narrow(poa_object.in());
        PortableServer::POAManager_var poa_manager = poa->the_POAManager ();    
        CORBA::PolicyList policies (1);
        policies.length (1);
        CORBA::Any pol;

        pol <<= BiDirPolicy::BOTH;
        policies[0] =
        orb->create_policy (BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE, 
                pol   
                ACE_ENV_ARG_PARAMETER);
        ACE_TRY_CHECK;

        // Create POA as child of RootPOA with the above policies.  This POA
        // will receive request in the same connection in which it sent
        // the request

        PortableServer::POA_var child_poa =
        poa->create_POA ("childPOA",
                      poa_manager.in (),
                      policies
                      ACE_ENV_ARG_PARAMETER);
        ACE_TRY_CHECK;

        // Creation of childPOA is over. Destroy the Policy objects.
        for( CORBA::ULong i = 0; i < policies.length (); ++i )
        {
            policies[i]->destroy (ACE_ENV_SINGLE_ARG_PARAMETER);
            ACE_TRY_CHECK;
        }
        poa_manager->activate (ACE_ENV_SINGLE_ARG_PARAMETER);
        ACE_TRY_CHECK;

        //Create the Servant
        unit_i = new Body_Unit_i(/*exec.in(),*/ UNITNAME, /*parser,*/ parent, this, std::string(workingDir) );
        unit_i_instantiated = true;
        //Activate it to obtain the object reference

        PortableServer::ObjectId_var id =
        PortableServer::string_to_ObjectId( CORBA::string_dup( UNITNAME.c_str() ) );
        child_poa->activate_object_with_id (id.in (),
                              unit_i
                              ACE_ENV_ARG_PARAMETER);

        //Activate it to obtain the object reference
        Body::Unit_var unit =  Body::Unit::_narrow(child_poa->id_to_reference (id.in ()
                        ACE_ENV_ARG_PARAMETER));
        ACE_TRY_CHECK;

        CosNaming::Name Unitname(1);
        Unitname.length(1);
        Unitname[0].id = CORBA::string_dup (UNITNAME.c_str());
        //Bind the object
        try
        {
            naming_context->bind(Unitname, unit.in());
        }
        catch(CosNaming::NamingContext::AlreadyBound& ex)
        {
            naming_context->rebind(Unitname, unit.in());
            std::cout << ex._info().c_str() << std::endl;
        }

        //Call the Executive CORBA call to register it to the Executive
        exec->RegisterUnit(unit_i->UnitName_.c_str(), unit.in(), 0); //0 means a normal module
    }
    catch (CORBA::Exception &)
    {
        std::cerr << "CORBA exception raised!" << std::endl;
    }
}
/////////////////////////////////////////////////////////////
void CorbaUnitManager::DestroyORB( void )
{	  
    if( !unit_i_instantiated )
    {
        return;
    }
    
    unit_i->CloseAspen();
    //Sleep(5000);
    
    CleanUp();
}
/////////////////////////////////////////////////////////////
Body_Unit_i* CorbaUnitManager::GetUnitObject( void )
{
   return unit_i;
}
/////////////////////////////////////////////////////////////
void CorbaUnitManager::CheckCORBAWork( void )
{
	if ( !CORBA::is_nil( orb ) )
   {
      if ( orb->work_pending() )
      {
         orb->perform_work();
      }
   }
}

/////////////////////////////////////////////////////////////
BKPParser * CorbaUnitManager::CreateParser( void )
//void CorbaUnitManager::CreateParser( void )
{
	return new BKPParser();
}

/////////////////////////////////////////////////////////////
//DynParser * CorbaUnitManager::CreateParser( void )
//void CorbaUnitManager::CreateParser( void )
//{
//	return new DynParser();
//}

/////////////////////////////////////////////////////////////
bool CorbaUnitManager::CleanUp( void )
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