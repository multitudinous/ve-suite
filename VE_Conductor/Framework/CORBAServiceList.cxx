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
   
   return true;
}
/////////////////////////////////////////////////////////////
bool CORBAServiceList::IsConnectedToCE( void )
{
   if ( CORBA::is_nil( module.in() ) )
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
      catch ( CORBA::Exception& ) 
      {
            frame->Log("Can't find executive or UI registration error\n");
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
      vjobs = VjObs::_narrow(ve_object.in());

      if ( CORBA::is_nil( vjobs.in() ) )
         std::cerr<<"VjObs is Nill"<<std::endl;
    
      //Create the VE Tab
      //con_menu->Enable(v21ID_DISCONNECT_VE, true);
      frame->Log("Connected to VE server.\n");
      //connectToVE = true;
      //network->SetXplorerInterface( vjobs.in() );
   } 
   catch (CORBA::Exception &) 
   {
      frame->Log("Can't find VE server\n");
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
   char* argv[ argc ];
   for ( int i = 0; i < argc; ++ i )
   {
      int stringLength = strlen( ::wxGetApp().argv[ i ] );
      argv[ i ] = new char[ stringLength + 1 ];
      strcpy(argv[ i ], ::wxGetApp().argv[ i ] );
   }

   try 
   {
      // First initialize the ORB, 
      orb = CORBA::ORB_init (argc, argv,""); // the ORB name, it can be anything! 
      //delete the left over char*
      for ( int i = 0; i < argc; ++ i )
      {
         delete [] argv[ i ];
      }
      
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
      frame->Log("Initialized ORB and connection to the Naming Service\n");
      return true;
   }
   catch ( CORBA::Exception& ) 
   {  
      //		poa->destroy (1, 1);
      // Finally destroy the ORB
      orb->destroy();
      frame->Log("CORBA exception raised! Can't init ORB or can't connect to the Naming Service\n");
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
    
      frame->Log("Disconnect successful.\n");
   }
   catch (CORBA::Exception &) 
   {
      frame->Log("Disconnect failed.\n");
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
   frame->Log("Disconnect VE suceeded.\n");

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
         try {
            frame->network->exec->RegisterUI( p_ui_i->UIName_.c_str(), ui.in());
            frame->con_menu->Enable(v21ID_SUBMIT,true);
            frame->con_menu->Enable(v21ID_LOAD, true);
            //frame_->con_menu->Enable(v21ID_CONNECT, false);
            frame->run_menu->Enable(v21ID_VIEW_RESULT, true);
            frame->con_menu->Enable(v21ID_DISCONNECT, true);
            
            //frame_->orb->run();
         }catch (CORBA::Exception &) {
            
            frame->Log("Can't find executive or UI registration error.\n");
         }
      }
      else
      {
         try {
            PortableServer::ObjectId_var idObject = PortableServer::string_to_ObjectId( CORBA::string_dup( UINAME.c_str() ) );
            
            //Activate it to obtain the object reference
            Body::UI_var ui = Body::UI::_narrow( poa->id_to_reference( idObject.in() ) );
            
            frame->network->exec->RegisterUI( p_ui_i->UIName_.c_str(), ui.in());
            frame->con_menu->Enable(v21ID_SUBMIT,true);
            frame->con_menu->Enable(v21ID_LOAD, true);
            frame->con_menu->Enable(v21ID_CONNECT, false);
            frame->run_menu->Enable(v21ID_VIEW_RESULT, true);
            frame->con_menu->Enable(v21ID_DISCONNECT, true);
            
         }catch (CORBA::Exception &) {
            
            frame->Log("Can't find executive or UI registration error.\n");
         }
      }
   }
   catch (CORBA::Exception &)
   {
      frame->Log("Can't find executive or UI registration error.\n");
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
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                       "Communication Failure", wxOK | wxICON_INFORMATION );
         return false;
      }
   }
   //Clean up memory
   //delete veCommand;
   return true;
}
/////////////////////////////////////////////////////////////////
VjObs_ptr CORBAServiceList::GetXplorerPointer( void )
{
   return vjobs.in();
}
/////////////////////////////////////////////////////////////////
PEThread* CORBAServiceList::GetMessageLog( void )
{
   return pelog;
}