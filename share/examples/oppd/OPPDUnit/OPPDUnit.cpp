// OPPDUnit.cpp : Defines the class behaviors for the application.

#include "stdafx.h"
#include "OPPDUnit_i.h"
#include "OPPDUnit.h"
//#include "DialogDlg.h"
#include "VEOpen/skel/moduleC.h"
#include "orbsvcs/CosNamingC.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

BEGIN_MESSAGE_MAP(COPPDUnitApp, CWinApp)
   ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

COPPDUnitApp::COPPDUnitApp()
{
}

// The one and only COPPDUnitApp object
COPPDUnitApp theApp;

const GUID CDECL BASED_CODE _tlid =
      { 0x58F4E9D3, 0x5693, 0x42A1, { 0xA4, 0xE9, 0x3D, 0x20, 0x6E, 0xD1, 0x1, 0xF } };
const WORD _wVerMajor = 1;
const WORD _wVerMinor = 0;

// COPPDUnitApp initialization
BOOL COPPDUnitApp::InitInstance()
{
   // InitCommonControls() is required on Windows XP if an application
   // manifest specifies use of ComCtl32.dll version 6 or later to enable
   // visual styles.  Otherwise, any window creation will fail.

   CWinApp::InitInstance();

   // Initialize OLE libraries
   if (!AfxOleInit())
   {
      AfxMessageBox(IDP_OLE_INIT_FAILED);
      return FALSE;
   }

   AfxEnableControlContainer();   

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
      //return 1;
      exit(1);
   }

   int argc;
   char** argv;
   // Look at inherited string to see if commandline args were supplied
   if ( m_lpCmdLine[0] == _T('\0') )
   {
      //AfxMessageBox("No Commandline args: using localhost");
      // If commandline has no arguments use localhost as the default...
      argc = 3;
      argv = new char*[ argc ];
      argv[ 0 ] = "project";
      argv[ 1 ] = "-ORBInitRef";
      argv[ 2 ] = "NameService=corbaloc:iiop:virtual2.inel.gov:1237/NameService";
   }
   else
   {
      //AfxMessageBox("Will use commandline args to determine machine and port number");
      // use commandline arguments for machine and port number...
      argc = 3;
      argv = new char*[ argc ];
      argv[ 0 ] = "project";

      char * pch = strtok (m_lpCmdLine," "); // separate words by spaces
      int i = 1;
      while (pch != NULL)
      {
         argv[ i ] = new char [ strlen(pch)+1 ];
         strcpy( argv[ i ], pch );
         //AfxMessageBox(argv[ i ]);
         pch = strtok (NULL, " ");
         i++;
      }
      if ( i != 3 )
         exit(1);
   }

   std::string UNITNAME = "OPPD";
   try {
      // First initialize the ORB, 
      CORBA::ORB_var orb =
      CORBA::ORB_init (argc, argv,
      "" /* the ORB name, it can be anything! */);

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

      Body::Executive_var exec = Body::Executive::_narrow(exec_object.in());

      // other client code

      //...........

      //Here is the code to set up the server
      CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
      PortableServer::POA_var poa = PortableServer::POA::_narrow(poa_object.in());
      PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
      poa_manager->activate ();

      //Create the Servant
      Body_Unit_i unit_i(exec.in(), UNITNAME);
      //Activate it to obtain the object reference
      Body::Unit_var unit = unit_i._this();

      CosNaming::Name Unitname(1);
      Unitname.length(1);
      Unitname[0].id = CORBA::string_dup (UNITNAME.c_str());
      //Bind the object
      try {
         naming_context->bind(Unitname, unit.in());
      }
      catch(CosNaming::NamingContext::AlreadyBound& ex){
         naming_context->rebind(Unitname, unit.in());
      }

      //Call the Executive CORBA call to register it to the Executive
      exec->RegisterUnit(unit_i.UnitName_.c_str(), unit.in(), 0); //0 means a normal module

      orb->run();
      // Destroy the POA, waiting until the destruction terminates
      poa->destroy (1, 1);
      // Finally destroy the ORB
      orb->destroy();
   }
   catch (CORBA::Exception &)
   {
      cerr << "CORBA exception raised!" << endl;
   }
   
   // Since the dialog has been closed, return FALSE so that we exit the
   //  application, rather than start the application's message pump.
   return FALSE;
}
