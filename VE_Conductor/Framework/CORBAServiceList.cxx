/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/CORBAServiceList.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Conductor/Framework/OrbThread.h"

#include <orbsvcs/CosNamingC.h>
#include "VE_Conductor/Framework/UI_i.h"
#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/Network.h"
#include "VE_Conductor/Framework/App.h"

#include <tao/BiDir_GIOP/BiDirGIOP.h>
#include <ace/SString.h>
#include <ace/SStringfwd.h>

#include <sstream>

#include <wx/wx.h>
#include <wx/app.h>
#include <wx/utils.h>

using namespace VE_XML;
using namespace VE_Conductor;

/////////////////////////////////////////////////////////////
CORBAServiceList::CORBAServiceList( AppFrame* frame )
{
   this->frame = frame;
   p_ui_i = 0;
   pelog = 0;
}
/////////////////////////////////////////////////////////////
CORBAServiceList::~CORBAServiceList( void )
{
   if ( pelog )
   {
      delete pelog;
   }
}
/////////////////////////////////////////////////////////////
void CORBAServiceList::SetNamingContext( CosNaming::NamingContext_ptr naming_context )
{
   namingContext = naming_context;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::IsConnectedToXplorer( void )
{
   if ( CORBA::is_nil( vjobs.in() ) )
   {
      return ConnectToXplorer();
   }

   try
   {
      vjobs->_non_existent();
   }
   catch (...)
   {
      return ConnectToXplorer();
   }
   
   return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::IsConnectedToCE( void )
{
   if ( CORBA::is_nil( module.in() ) )
   {
      return ConnectToCE();
   }
   
   try
   {
      module->_non_existent();
   }
   catch (...)
   {
      return ConnectToCE();
   }
   
   return true;
}
/////////////////////////////////////////////////////////////
std::vector< std::string > CORBAServiceList::GetListOfServices( void )
{
   unsigned long numServices;
   //namingContext.list( numServices, bindList, nameList );
   //Need to look at CORBA book for for loop
   return serviceList;
}   
/////////////////////////////////////////////////////////////
bool CORBAServiceList::ConnectToCE( void )
{
   if ( pelog == NULL )
   {
	   pelog = new PEThread( frame );
	   pelog->activate();
   }

   if ( !IsConnectedToNamingService() )
   {
      return false;
   }

   if ( p_ui_i == 0 )
   {
      try
      {   
         CreateCORBAModule();
      } 
      catch ( CORBA::Exception& ex ) 
      {
         GetMessageLog()->SetMessage( "Can't find executive or UI registration error\n");
         return false;
      }
   }
   return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::ConnectToXplorer( void )
{
   if ( pelog == NULL )
   {
	   pelog = new PEThread( frame );
	   pelog->activate();
   }
   
   if ( !IsConnectedToNamingService() )
   {
      return false;
   }
   
   try 
   {
      CosNaming::Name name(1);
      name.length(1);
      //Now get the reference of the VE server
      name[0].id   = CORBA::string_dup ("Master");
      name[0].kind = CORBA::string_dup ("VE_Xplorer");
      CORBA::Object_var naming_context_object =
      orb->resolve_initial_references ("NameService");
      CosNaming::NamingContext_var naming_context1 = CosNaming::NamingContext::_narrow (naming_context_object.in ());
      CORBA::Object_var ve_object = naming_context1->resolve(name);
      vjobs = VjObs::_narrow( ve_object.in() );

      //Create the VE Tab
      //con_menu->Enable(v21ID_DISCONNECT_VE, true);
      GetMessageLog()->SetMessage( "Connected to VE server.\n");
      //connectToVE = true;
      //network->SetXplorerInterface( vjobs.in() );
   } 
   catch ( CORBA::Exception& ex ) 
   {
      GetMessageLog()->SetMessage( "Can't find VE server\n");
      GetMessageLog()->SetMessage( ex._info().c_str() );
      return false;
   }
  
   return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::IsConnectedToNamingService( void )
{
   if ( CORBA::is_nil( naming_context.in() ) )
   {
      return ConnectToNamingService();
   }
   
   return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::ConnectToNamingService( void )
{
   //Copy the command line args because tao deletes them after processing them
   int argc = ::wxGetApp().argc;
   char** argv = new char*[ argc ];
   for ( int i = 0; i < argc; ++ i )
   {
      int stringLength = strlen( ::wxGetApp().argv[ i ] );
      argv[ i ] = new char[ stringLength + 1 ];
      strcpy(argv[ i ], ::wxGetApp().argv[ i ] );
   }

   try 
   {
      // First initialize the ORB, 
      orb = CORBA::ORB_init( argc, argv,""); // the ORB name, it can be anything! 
      //delete the left over char*
      for ( int i = 0; i < argc; ++ i )
      {
         delete [] argv[ i ];
         argv[i] = 0;
      }
      delete [] argv;
      argv = 0;
      
      //Here is the part to contact the naming service and get the reference for the executive
      CORBA::Object_var naming_context_object =
         orb->resolve_initial_references ("NameService");
      naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());
      /*if (naming_context==CORBA)
      {
         poa->destroy (1, 1);
		    // Finally destroy the ORB
		    orb->destroy();
		    std::cerr << "Naming service not found!" << std::endl;
		    return false;
		    }
      */
      GetMessageLog()->SetMessage( "Initialized ORB and connection to the Naming Service\n");
      
      return true;
   }
   catch ( CORBA::Exception& ex ) 
   {  
      //		poa->destroy (1, 1);
      // Finally destroy the ORB
      orb->destroy();
      GetMessageLog()->SetMessage( "CORBA exception raised! Can't init ORB or can't connect to the Naming Service\n");
      GetMessageLog()->SetMessage( ex._info().c_str() );
      return false;
   }
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::DisconnectFromCE( void )
{
   try
   {
      frame->network->exec->UnRegisterUI(p_ui_i->UIName_.c_str());
      delete p_ui_i;
      p_ui_i = NULL;

      //con_menu->Enable(v21ID_SUBMIT,false);
      //con_menu->Enable(v21ID_LOAD, false);
      //con_menu->Enable(v21ID_CONNECT, true);
      frame->run_menu->Enable(v21ID_START_CALC, false);
      // EPRI TAG run_menu->Enable(v21ID_VIEW_RESULT, false);
      frame->con_menu->Enable(v21ID_DISCONNECT, false);
    
      GetMessageLog()->SetMessage( "Disconnect successful.\n");
   }
   catch (CORBA::SystemException& ex ) 
   {
      GetMessageLog()->SetMessage( "Disconnect failed.\n");
      GetMessageLog()->SetMessage( ex._info().c_str() );
      return false;
   }

   return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::DisconnectFromXplorer( void )
{
   // delete m_frame;
   //m_frame = NULL;
   frame->con_menu->Enable(v21ID_DISCONNECT_VE, false);
/*
   if ( navPane )
   {
      navPane->Close( false );
   }

   if ( viewlocPane )
   {
      viewlocPane->Close( false );
   }

   if ( streamlinePane )
   {
	   streamlinePane->Close( false );
   }

   if ( soundsPane )
   {
      soundsPane->Close( false );
   }
*/
   GetMessageLog()->SetMessage( "Disconnect VE suceeded.\n");

   //connectToVE = false;
   return true;
}
/////////////////////////////////////////////////////////////
void CORBAServiceList::CheckORBWorkLoad( void )
{
   try
   {
      if ( !CORBA::is_nil( orb.in() ) )
      {
         ::wxMilliSleep( 250 );
         if ( orb->work_pending() )
         {
            orb->perform_work();
         }      
      }
   }
   catch ( ... )
   {
      
   }
}
/////////////////////////////////////////////////////////////
void CORBAServiceList::CreateCORBAModule( void )
{
   try
   {
      long id = time(NULL);
      //char* uiname;
      //sprintf(uiname, "UIClient%ld", id);
      std::ostringstream dirStringStream;
      dirStringStream << "UIClient" << id;
      std::string UINAME = dirStringStream.str();
      //uiname = (char*)dirString.c_str();
      //std::string UINAME = uiname;
      CosNaming::Name name(1);
      name.length(1);
      name[0].id = CORBA::string_dup ("Executive");
      
      CORBA::Object_var naming_context_object = orb->resolve_initial_references ("NameService");
      naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());
      
      CORBA::Object_var exec_object = naming_context->resolve(name);
      frame->network->exec = Body::Executive::_narrow(exec_object.in());
      module = Body::Executive::_duplicate( frame->network->exec.in() );
      
      //Create the Servant
      if ( p_ui_i == NULL )
      {
         p_ui_i= new Body_UI_i(frame->network->exec.in(), UINAME);
         
         //pass the Frame's pointer to the UI corba implementation
         p_ui_i->SetUIFrame( frame );
         //Here is the code to set up the ROOT POA
         CORBA::Object_var poa_object = orb->resolve_initial_references ("RootPOA"); // get the root poa
         poa_root = PortableServer::POA::_narrow(poa_object.in());
         PortableServer::POAManager_var poa_manager = poa_root->the_POAManager ();
         
         CORBA::PolicyList policies (1);
         policies.length (1);
         
         CORBA::Any pol;
         pol <<= BiDirPolicy::BOTH;
         policies[0] =
            orb->create_policy (BiDirPolicy::BIDIRECTIONAL_POLICY_TYPE,
                                        pol);
         
         // Create POA as child of RootPOA with the above policies.  This POA
         // will receive request in the same connection in which it sent
         // the request
         try
         {
            poa = poa_root->create_POA ("childPOA",
                                                        poa_manager.in (),
                                                        policies);
         }
         catch (const PortableServer::POA::AdapterAlreadyExists & )
         {
            std::cout << " Child POA Already Connected : Do nothing " << std::endl;
         }
         
         // Creation of childPOA is over. Destroy the Policy objects.
         for (CORBA::ULong i = 0; i < policies.length (); ++i)
         {
            policies[i]->destroy ();
         }
         
         poa_manager->activate();
         PortableServer::ObjectId_var idObject = PortableServer::string_to_ObjectId (CORBA::string_dup (UINAME.c_str()));
         poa->activate_object_with_id( idObject.in() , p_ui_i );
         
         //Activate it to obtain the object reference
         Body::UI_var ui = Body::UI::_narrow( poa->id_to_reference( idObject.in() ) );
         
         //CosNaming::Name UIname(1);
         //UIname.length(1);
         //UIname[0].id = CORBA::string_dup (UINAME.c_str());
         
         //Bind the object
         //try   {
         //      frame_->naming_context->bind(UIname, ui.in());
         //   }catch(CosNaming::NamingContext::AlreadyBound& ex){
         //      frame_->naming_context->rebind(UIname, ui.in());
         //   }
         
         //(frame_->_mutex).release();
         try 
         {
            frame->network->exec->RegisterUI( p_ui_i->UIName_.c_str(), ui.in());
            frame->con_menu->Enable(v21ID_SUBMIT,true);
            frame->con_menu->Enable(v21ID_LOAD, true);
            //frame_->con_menu->Enable(v21ID_CONNECT, false);
            frame->run_menu->Enable(v21ID_VIEW_RESULT, true);
            frame->con_menu->Enable(v21ID_DISCONNECT, true);
            //frame_->orb->run();
         }
         catch ( CORBA::Exception& ex ) 
         {
            GetMessageLog()->SetMessage( "Can't find executive or UI registration error.\n" );
            GetMessageLog()->SetMessage( ex._info().c_str() );
         }
      }
      else
      {
         try 
         {
            PortableServer::ObjectId_var idObject = PortableServer::string_to_ObjectId( CORBA::string_dup( UINAME.c_str() ) );
            
            //Activate it to obtain the object reference
            Body::UI_var ui = Body::UI::_narrow( poa->id_to_reference( idObject.in() ) );
            
            frame->network->exec->RegisterUI( p_ui_i->UIName_.c_str(), ui.in());
            frame->con_menu->Enable(v21ID_SUBMIT,true);
            frame->con_menu->Enable(v21ID_LOAD, true);
            frame->con_menu->Enable(v21ID_CONNECT, false);
            frame->run_menu->Enable(v21ID_VIEW_RESULT, true);
            frame->con_menu->Enable(v21ID_DISCONNECT, true);
            
         }
         catch (CORBA::Exception& ex ) 
         {
            GetMessageLog()->SetMessage( "Can't find executive or UI registration error.\n");
            GetMessageLog()->SetMessage( ex._info().c_str() );
         }
      }
   }
   catch (CORBA::Exception& ex )
   {
      GetMessageLog()->SetMessage( "Can't find executive or UI registration error.\n");
      GetMessageLog()->SetMessage( ex._info().c_str() );
   }
}
//////////////////////////////////////////////////
bool CORBAServiceList::SendCommandStringToXplorer( VE_XML::Command* veCommand )
{
   if ( !IsConnectedToXplorer() )
      return false;
      
   //Now send the data to xplorer
   VE_XML::XMLReaderWriter netowrkWriter;
   netowrkWriter.UseStandaloneDOMDocumentManager();

   // Create the command and data value pairs
   //VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair();
   //dataValuePair->SetData( "CREATE_NEW_DATASETS", veModel );
   //VE_XML::Command* veCommand = new VE_XML::Command();
   //veCommand->SetCommandName( std::string("UPDATE_MODEL_DATASETS") );
   //veCommand->AddDataValuePair( dataValuePair );

   // New need to destroy document and send it
   std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
   nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( veCommand, "vecommand" ) );
   std::string xmlDocument( "returnString" );
   netowrkWriter.WriteToString();
   netowrkWriter.WriteXMLDocument( nodes, xmlDocument, "Command" );

   if ( !CORBA::is_nil( vjobs.in() ) && !xmlDocument.empty() )
   {
      try
      {
         // CORBA releases the allocated memory so we do not have to
         vjobs->SetCommandString( CORBA::string_dup( xmlDocument.c_str() ) );
      }
      catch ( ... )
      {
         //VjObs::_tao_release( vjobs );
         //if ( !IsConnectedToXplorer() )
         {  
            //wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
            //              "Communication Failure", wxOK | wxICON_INFORMATION );
            return false;
         }
      }
   }
   //Clean up memory
   //delete veCommand;
   return true;
}
////////////////////////////////////////////////////////////////////////////////
VjObs_ptr CORBAServiceList::GetXplorerPointer( void )
{
   return vjobs.in();
}
////////////////////////////////////////////////////////////////////////////////
PEThread* CORBAServiceList::GetMessageLog( void )
{
   if ( pelog == NULL )
   {
	   pelog = new PEThread( frame );
	   pelog->activate();
   }
   
   return pelog;
}
////////////////////////////////////////////////////////////////////////////////
bool CORBAServiceList::SetID( int moduleId, std::string moduleName )
{
   if ( !CORBAServiceList::IsConnectedToCE() )
   {
      return false;
   }
   
   try
   {
      module->SetID( CORBA::string_dup( moduleName.c_str() ), moduleId );
   }
   catch ( ... )
   {
      return false;
   }
   return true;
}
