#include "UI_Frame.h"
#include <iostream>
using namespace std;
#ifdef _TAO
#include "VjObsC.h"
#include "VjObsS.h"
#include <orbsvcs/CosNamingC.h>
#else
#include "VjObs.h"
#endif
#include "cfdEnum.h"


////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
UI_Frame::UI_Frame(wxWindow* parent, wxWindowID id,
             const wxPoint& pos,
             const wxSize& size,
             long style)
: wxPanel(parent, -1, pos, size, style)
{
  
   //_appParent = new wxString( "DirectVE" );
   _appParent = wxT( "DirectVE" );
   buildCORBA();

   _modelData = new UI_ModelData(vjobs.in());
   
   buildFrame();

}

UI_Frame::UI_Frame(VjObs_ptr ref,wxWindow* parent, wxWindowID id, 
            const wxPoint& pos ,
            const wxSize& size ,
            long style )
:wxPanel(parent, id, pos, size, style)
{
   
   vjobs = VjObs::_duplicate(ref);
   _modelData = new UI_ModelData(vjobs.in());
   buildFrame();
   //_appParent = new wxString( "Framework" );
   _appParent = wxT( "Framework" );
}


void UI_Frame::buildCORBA( )
{
     
   //NOTE:New controls that are added to the frame that
   //aren't related(located on) the tabs should be initialized
   //here!!!!Also their sizers should be added here!!!
   //make sure to create the associated panel for the new
   //contol, otherwise it will be under the control of the
   //tabs panel(not recommened, resizing issues may arise).

	char *argv[]={""};
	int argc = 0;
   PortableServer::POA_var poa;
   CORBA::ORB_var orb;
   CosNaming::NamingContext_var naming_context;
   //VjObs_var vjobs;
   
   try 
     {
       // First initialize the ORB, 
       orb =
	 CORBA::ORB_init (argc, argv,
			  ""); // the ORB name, it can be anything! 
       
       //Here is the code to set up the ROOT POA
       CORBA::Object_var poa_object =
	 orb->resolve_initial_references ("RootPOA"); // get the root poa
       
       poa = PortableServer::POA::_narrow(poa_object.in());
       PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
       poa_manager->activate();
       
       
       //Here is the part to contact the naming service and get the reference for the executive
       CORBA::Object_var naming_context_object =
	 orb->resolve_initial_references ("NameService");
       
       naming_context = CosNaming::NamingContext::_narrow (naming_context_object.in ());
     }  
   catch (CORBA::Exception &) 
     {
       poa->destroy (1, 1);
       // Finally destroy the ORB
       orb->destroy();
       cerr << "CORBA exception raised!" << endl;
     }
   
   try 
     {
       
       CosNaming::Name name(1);
       name.length(1);
       //Now get the reference of the VE server
       name[0].id   = CORBA::string_dup("Master");
       name[0].kind = CORBA::string_dup("VE_Xplorer");
	   CORBA::Object_var ve_object;
	   try
	   { 
		   if ( !CORBA::is_nil( naming_context.in() ) )
		      ve_object = naming_context->resolve(name);
	   }
	   catch ( CORBA::Exception & )
	   {
	      cout << " Can't resolve name " <<endl;
	   }

	   if ( !CORBA::is_nil( ve_object.in() ) )
          vjobs = VjObs::_narrow(ve_object.in());
       
	   if (CORBA::is_nil(vjobs.in()))
	      std::cerr<<"VjObs is Nill"<<std::endl;
       
     } 
   catch (CORBA::Exception &) 
     {
       
       cerr << "Can't find VE server" << endl;
     }
}    
 
void UI_Frame::buildFrame( )
{
    
   _tabs = 0;
   _datasetPanel = 0;
   _modselPanel = 0;
   activeModIndex = 0;
   //the tabs of our UI

   //_tabs = new UI_Tabs( vjobs.in(), this, ID_UI_TABS);
   _tabs = new UI_Tabs( vjobs.in(), this, _modelData, activeModIndex);

   //set the left side of the gui
   
   _datasetPanel = new UI_DatasetPanel(this, _modelData, activeModIndex);
   
   
   //create the individual pages for the tab control
   _tabs->createTabPages();

   //the frame sizer
   _frameSizer = new wxBoxSizer(wxHORIZONTAL);

   //the notebook sizer
   _tabsSizer = new wxNotebookSizer(_tabs);

   //the panel sizers for datasetPage and scalartab
   _datasetSizer = new wxBoxSizer(wxHORIZONTAL);
   //wxBoxSizer* _scalarSizer = new wxBoxSizer(wxHORIZONTAL);
   _datasetSizer->Add(_datasetPanel,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_scalarSizer->Add(_scalartab,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _modselPanel = new UI_ModSelPanel(this, _modelData);
      
   _tabs->cSc = activeModIndex;         // using zero-based scalar counting
   _tabs->cId  = CHANGE_ACTIVE_MODEL;
   _tabs->sendDataArrayToServer();

   _modselSizer = new wxBoxSizer(wxHORIZONTAL);
   _modselSizer->Add(_modselPanel,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //add the tabs to the frame
   //NOTE: This is where the layout of the UI 
   //should be handled when adding new controls!!
   //_frameSizer->Add(_datasetSizer,3,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_frameSizer->Add(_scalarSizer,8,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_frameSizer->Add(_tabsSizer,22,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _frameSizer->Add(_modselSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _frameSizer->Add(_datasetSizer,4,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_frameSizer->Add(_scalarSizer,3,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _frameSizer->Add(_tabsSizer,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //refresh the layout
   _frameSizer->Layout();

   //set the sizer of this frame
   SetSizer(_frameSizer);

   //Auto"magic" resizing
   //SetAutoLayout(TRUE);
   SetAutoLayout(true);

   //Tell the sizer to resize the window to
   // match the sizer's minimal size
   _frameSizer->Fit(this); 
}
/////////////////////
//Destructor       //
/////////////////////
UI_Frame::~UI_Frame()
{
	_tabsSizer->Remove(_tabs);
	_datasetSizer->Remove(_datasetPanel);
	_frameSizer->Remove(_tabsSizer);
	_frameSizer->Remove(_datasetSizer);

	delete _tabs;
	delete _datasetPanel;
	
}

void UI_Frame::OnChangeModel( void )
{
  
   _datasetPanel->_rebuildDataSets( activeModIndex );
   _tabs->DeleteAllPages();   
   _tabs->rebuildTabPages( activeModIndex );
   cout<<"Act Mod Index2: "<<activeModIndex<<endl;

   _tabs->cSc = activeModIndex;         // using zero-based scalar counting
   cout<<"Act Mod Index1: "<<activeModIndex<<endl;
   _tabs->cId  = CHANGE_ACTIVE_MODEL;
   _tabs->sendDataArrayToServer();
 
   //Refresh();
   // Hack because Refresh and SetSize(GetSize() ) don't work on win32 platform
   static bool test = false;
   int flag = 0;
   if ( test )
   {
      flag = 1;
	  test = false;
   }
   else
   {
      flag = -1;
	  test = true;
   }
   
   wxSize temp = GetSize();
   temp.SetHeight( temp.GetHeight()+flag );
   temp.SetWidth( temp.GetWidth()+flag );
   SetSize( temp );
   //((wxWindow*)GetParent())->SetSize( ((wxWindow*)GetParent())->GetSize() );  
}
