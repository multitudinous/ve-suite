// OrbThread.cpp: implementation of the OrbThread class.
//
//////////////////////////////////////////////////////////////////////
#include "OrbThread.h"
#include "Frame.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

OrbThread::OrbThread(AppFrame* frame)
: wxThread(wxTHREAD_JOINABLE)
{
	frame_ = frame;
	Create();
}

OrbThread::~OrbThread()
{

}

bool OrbThread::Do() 
//int OrbThread::svc (void)
{
	long id = time(NULL);
	char uiname[256];
	sprintf(uiname, "UIClient%ld", id);
	std::string UINAME = uiname;
	CosNaming::Name name(1);
	name.length(1);
	name[0].id = CORBA::string_dup ("Executive");
   
	CORBA::Object_var exec_object = frame_->naming_context->resolve(name);
	frame_->network->exec = Body::Executive::_narrow(exec_object.in());
	//Create the Servant
	if (frame_->p_ui_i==NULL)
		frame_->p_ui_i= new Body_UI_i(frame_->network->exec.in(), UINAME);
	else
		UINAME=frame_->p_ui_i->UIName_.c_str();
   
	//pass the Frame's pointer to the UI corba implementation
	frame_->p_ui_i->SetUIFrame(frame_);
		//Here is the code to set up the ROOT POA
	CORBA::Object_var poa_object =
	frame_->orb->resolve_initial_references ("RootPOA"); // get the root poa
   
	frame_->poa = PortableServer::POA::_narrow(poa_object.in());
	PortableServer::POAManager_var poa_manager = frame_->poa->the_POAManager ();
	poa_manager->activate (); //activate the root POA

	//Activate it to obtain the object reference
	Body::UI_var ui = (*(frame_->p_ui_i))._this();
     
	CosNaming::Name UIname(1);
	UIname.length(1);
	UIname[0].id = CORBA::string_dup (UINAME.c_str());
   
	//Bind the object
	try	{
			frame_->naming_context->bind(UIname, ui.in());
		}catch(CosNaming::NamingContext::AlreadyBound& ex){
			frame_->naming_context->rebind(UIname, ui.in());
		}
	//(frame_->_mutex).release();
	try {
	frame_->network->exec->RegisterUI(frame_->p_ui_i->UIName_.c_str());
	frame_->orb->run();
		}catch (CORBA::Exception &) {
		
		frame_->logwindow->AppendText("Can't find executive or UI registration error.\n");
	}

	return true;
}