/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "stdafx.h"
#include "AdiabaticFlameTempUnit_i.h"
#include "AdiabaticFlameTempUnit_client.h"
#include "moduleC.h"
#include "orbsvcs/CosNamingC.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

BEGIN_MESSAGE_MAP(CAdiabaticFlameTempApp, CWinApp)
   ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

CAdiabaticFlameTempApp::CAdiabaticFlameTempApp()
{
}

// The one and only CAdiabaticFlameTempApp object
CAdiabaticFlameTempApp theApp;

const GUID CDECL BASED_CODE _tlid =
      { 0x58F4E9D3, 0x5693, 0x42A1, { 0xA4, 0xE9, 0x3D, 0x20, 0x6E, 0xD1, 0x1, 0xF } };
const WORD _wVerMajor = 1;
const WORD _wVerMinor = 0;

// CAdiabaticFlameTempApp initialization
BOOL CAdiabaticFlameTempApp::InitInstance()
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

   int argc = 1;
   char** argv;
   std::vector< char* > cmdargs;

   char* pch = strtok( m_lpCmdLine, " " ); // separate words by spaces

   cmdargs.push_back( "project" );

   while ( pch != NULL )
   {     
      cmdargs.push_back( pch );
      pch = strtok( NULL, " " );
      argc++;
   }

   argv = new char*[ argc ];

   for ( int i=0; i<argc; i++ )
   {
      argv[ i ] = new char [ strlen( cmdargs.at( i ) ) + 1 ];
      strcpy( argv[ i ], cmdargs.at( i ) );
   }

   std::string UNITNAME = "AdiabaticFlameTemp";
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
