#include "Frame.h"
#include "UI_i.h"
#include "package.h"

  
// Implementation skeleton constructor
Body_UI_i::Body_UI_i (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
  {
    UIName_=name;
  }
  
// Implementation skeleton destructor
Body_UI_i::~Body_UI_i (void)
  {
  }
  
void Body_UI_i::UpdateNetwork (
    const char * network
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (network!=NULL)
      std::cout<<network<<std::endl;
    std::cout<<UIName_<<" :UpdateNetwork called"<<std::endl;
    
    ui_network_->LoadS(network);
     
  }
  
void Body_UI_i::UpdateModuleUI (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (msg!=NULL)
      std::cout<<module_id<<" : "<<msg<<std::endl;
    std::cout<<UIName_<<" :UpdateModuleUI called"<<std::endl;

    //Real code the update UI
    Package p;
    p.SetSysId("moduleUI.xml");
    p.Load(msg, strlen(msg));

    ui_network_->s_mutexProtect.Lock();
    ui_network_->modules[module_id].pl_mod->UnPack(&(p.intfs[0]));
    ui_network_->s_mutexProtect.Unlock();
    ui_network_->Refresh();
  }
  
void Body_UI_i::UpdateModuleResult (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (msg!=NULL)
      std::cout<<module_id<<" : "<<msg<<std::endl;
    std::cout<<UIName_<<" :UpdateModuleResult called"<<std::endl;
  }
  
void Body_UI_i::UpdateLinkContent (
    CORBA::Long id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (msg!=NULL)
      std::cout<<id<<" : "<<msg<<std::endl;
    std::cout<<UIName_<<" :UpdateLinkContent called"<<std::endl;
  }
  
void Body_UI_i::Raise (
    const char * notification
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
	
    if (string(notification)!="")
      frame_->logwindow->AppendText(notification);
  }
  
void Body_UI_i::SetUIFrame(AppFrame* frame)
 { 
	  frame_ = frame;
	  ui_network_=frame_->network; 
  }
