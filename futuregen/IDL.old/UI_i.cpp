#include "UI_i.h"

  
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
    if (notification!=NULL)
      std::cout<<notification<<std::endl;
    std::cout<<UIName_<<" :Raise called"<<std::endl;
  }
  
