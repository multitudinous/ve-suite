#include "UI_Frame.h"
#include <orbsvcs/CosNamingC.h>
#include <iostream>
using namespace std;
#include "VjObsC.h"
#include "VjObsS.h"

////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
UI_Frame::UI_Frame(const wxString& title,
             const wxPoint& pos,
             const wxSize& size,
             long style)
: wxFrame((wxWindow *) NULL, -1, title, pos, size, style)
{
   buildCORBA();
   buildFrame();
}

UI_Frame::UI_Frame(VjObs_ptr ref,wxWindow* parent, wxWindowID id, const wxString test,
            const wxPoint& pos ,
            const wxSize& size ,
            long style )
:wxFrame(parent, id, test, pos, size, style)
{
   //vjobs = VjObs::_duplicate(ref);
   buildFrame();
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
       name[0].id   = (const char*) "Master";
       name[0].kind = (const char*) "VE_Xplorer";
	   CORBA::Object_var ve_object;
	   try
	   { 
		   if ( !CORBA::is_nil( naming_context ) )
		      ve_object = naming_context->resolve(name);
	   }
	   catch ( CORBA::Exception & )
	   {
	      cout << " Can't resolve name " <<endl;
	   }

	   if ( !CORBA::is_nil( ve_object ) )
          vjobs = VjObs::_narrow(ve_object.in());
       
	   if (CORBA::is_nil(vjobs))
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
   _datasetScrollable = 0;
   //the tabs of our UI
   _tabs = new UI_Tabs( vjobs.in(), this, ID_UI_TABS);
   //_tabs = new UI_Tabs(this,-1);
   std::cout<<"testing"<<std::endl;
   //set the left side of the gui
   _datasetScrollable = new UI_DatasetScrollable(this);
   //_datasetPage = new UI_DatasetTab(this);
   std::cout<<"testing1"<<std::endl;
   //_scalartab = new UI_ScalarTab(this);
   
   //create the individual pages for the tab control
   _tabs->createTabPages();

   //the frame sizer
   wxBoxSizer* _frameSizer = new wxBoxSizer(wxHORIZONTAL);

   //the notebook sizer
   wxNotebookSizer* _tabsSizer = new wxNotebookSizer(_tabs);

   //the panel sizers for datasetPage and scalartab
   wxBoxSizer* _datasetSizer = new wxBoxSizer(wxHORIZONTAL);
   //wxBoxSizer* _scalarSizer = new wxBoxSizer(wxHORIZONTAL);
   _datasetSizer->Add(_datasetScrollable,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_scalarSizer->Add(_scalartab,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //add the tabs to the frame
   //NOTE: This is where the layout of the UI 
   //should be handled when adding new controls!!
   //_frameSizer->Add(_datasetSizer,3,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_frameSizer->Add(_scalarSizer,8,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_frameSizer->Add(_tabsSizer,22,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   _frameSizer->Add(_datasetSizer,3,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //_frameSizer->Add(_scalarSizer,3,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   _frameSizer->Add(_tabsSizer,5,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
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
}

void UI_Frame::changeActiveScalarOnDataset(const char* name)
{
   /*if(name){
      if(_datasetPage){
         _datasetPage->makeActiveScalarOnDataset(name);
      } 
   }*/
}
