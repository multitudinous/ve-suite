#include "VE_Conductor/Framework/CORBAServiceList.h"

//#include "VE_Installer/installer/installerImages/ve_xplorer_banner.xpm"


/////////////////////////////////////////////////////////////
CORBAServiceList::CORBAServiceList( void )
//:wxDialog(NULL,-1, wxString("CORBA Service List Pane") )
{
   //this->SetIcon( wxIcon( ve_xplorer_banner_xpm ) );
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
std::vector< std::string > CORBAServiceList::GetListOfServices( void )
{
   unsigned long numServices;
   namingContext.list( numServices, bindList, nameList );
   //Need to look at CORBA book for for loop
   return serviceList;
}   
void AppFrame::ConExeServer( void )
{
   if ( connectToCE )
      return;

   //have we already connected
   /*wxSplashScreen* splash = 0;
   wxImage splashImage(ve_ce_banner_xpm);
   wxBitmap bitmap(splashImage);
   splash = new wxSplashScreen(bitmap,
            wxSPLASH_CENTRE_ON_PARENT|wxSPLASH_TIMEOUT,
            2500, this, -1, wxDefaultPosition, wxDefaultSize,
            wxSIMPLE_BORDER|wxSTAY_ON_TOP);*/

   //wxSafeYield();
   if ( pelog == NULL )
   {
	   pelog = new PEThread(this);
	   pelog->activate();
   }

   if ( !is_orb_init )
   {
      is_orb_init = init_orb_naming();
   }

   try
   { 
      //_mutex.acquire();	  
      ot = new OrbThread(this);
      ot->activate();
      //ot->Run();
      //register it to the server
      //_mutex.acquire();
    
      //_mutex.release();
      //Enalbe Menu items
      connectToCE = true;
   } 
   catch ( CORBA::Exception& ) 
   {
      Log("Can't find executive or UI registration error\n");
   }

   ::wxMilliSleep( 2500 );
   //delete splash;
}
  
void AppFrame::ConVEServer( void )
{
   if ( connectToVE )
      return;

   if ( pelog == NULL )
   {
	   pelog = new PEThread(this);
	   pelog->activate();
   }

   if ( !is_orb_init )
   {
      is_orb_init = init_orb_naming();
   }

   /*wxImage splashImage(ve_xplorer_banner_xpm);
   wxBitmap bitmap(splashImage);
   wxSplashScreen* splash = new wxSplashScreen(bitmap,
            wxSPLASH_CENTRE_ON_PARENT|wxSPLASH_TIMEOUT,
            2500, this, -1, wxDefaultPosition, wxDefaultSize,
            wxSIMPLE_BORDER|wxSTAY_ON_TOP);*/
   //wxSafeYield();
  
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
      con_menu->Enable(v21ID_DISCONNECT_VE, true);
      Log("Connected to VE server.\n");
      connectToVE = true;
      network->SetXplorerInterface( vjobs.in() );
   } 
   catch (CORBA::Exception &) 
   {
      Log("Can't find VE server\n");
   }
  
   ::wxMilliSleep( 2500 );
   //delete splash;
}

bool AppFrame::init_orb_naming()
{
   try 
   {
      // First initialize the ORB, 
      orb = CORBA::ORB_init (wxGetApp().argc, wxGetApp().argv,
                       ""); // the ORB name, it can be anything! 
    
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
      Log("Initialized ORB and connection to the Naming Service\n");
      return true;
   }
   catch ( CORBA::Exception& ) 
   {  
      //		poa->destroy (1, 1);
      // Finally destroy the ORB
      orb->destroy();
      Log("CORBA exception raised! Can't init ORB or can't connect to the Naming Service\n");
      return false;
   }
}
void AppFrame::DisConExeServer(wxCommandEvent &WXUNUSED(event))
{
   try
   {
      network->exec->UnRegisterUI(p_ui_i->UIName_.c_str());
      delete p_ui_i;
      p_ui_i = NULL;

      //con_menu->Enable(v21ID_SUBMIT,false);
      //con_menu->Enable(v21ID_LOAD, false);
      //con_menu->Enable(v21ID_CONNECT, true);
      run_menu->Enable(v21ID_START_CALC, false);
      // EPRI TAG run_menu->Enable(v21ID_VIEW_RESULT, false);
      con_menu->Enable(v21ID_DISCONNECT, false);
    
      Log("Disconnect successful.\n");
   }
   catch (CORBA::Exception &) 
   {
      Log("Disconnect failed.\n");
   }

   connectToCE = false;
}

void AppFrame::DisConVEServer(wxCommandEvent &WXUNUSED(event))
{
   delete m_frame;
   m_frame = NULL;
   con_menu->Enable(v21ID_DISCONNECT_VE, false);

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

   Log("Disconnect VE suceeded.\n");

   connectToVE = false;
}
