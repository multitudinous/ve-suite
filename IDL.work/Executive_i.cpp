#include "Executive_i.h"
  
// Implementation skeleton constructor
Body_Executive_i::Body_Executive_i (CosNaming::NamingContext_ptr nc)
  : naming_context_(CosNaming::NamingContext::_duplicate(nc))
  {
  }
  
// Implementation skeleton destructor
Body_Executive_i::~Body_Executive_i (void)
  {
  }
  
char * Body_Executive_i::GetImportData (
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::map<CORBA::Long, MODULE_DATA>::iterator iter;
    
    iter = module_data_.find(module_id);
    if (iter!=module_data_.end() && port_id > 0)
      return CORBA::string_dup(((iter->second).data[port_id]).c_str());
    else
      return CORBA::string_dup("NOTHING");
    
  }
  
void Body_Executive_i::SetExportData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    const char * data
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::map<CORBA::Long, MODULE_DATA>::iterator iter;
    
    iter = module_data_.find(module_id);
    if (iter!=module_data_.end())
      (iter->second).data[port_id]=std::string(data);
  }
  
char * Body_Executive_i::GetExportData (
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::map<CORBA::Long, MODULE_DATA>::iterator iter;
    
    iter = module_data_.find(module_id);
    if (iter!=module_data_.end())
      return CORBA::string_dup(((iter->second).data[port_id]).c_str());
    return NULL;
  }
    
  
void Body_Executive_i::SetModuleMessage (
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
    std::map<CORBA::Long, MODULE_DATA>::iterator iter;
    
    iter = module_data_.find(module_id);
    if (iter!=module_data_.end())
      (iter->second).msg=std::string(msg);
  }
  
void Body_Executive_i::SetNetwork (
    const char * network
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    network_=network;
    std::cout<<network<<std::endl;
  }
  
char * Body_Executive_i::GetNetwork ( 
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    return CORBA::string_dup(network_.c_str());
    
    // Add your implementation here
  }
  
void Body_Executive_i::SetWatchList (
    const Types::ArrayLong & id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    watch_list_ = id;
  }
  
::Types::ArrayLong * Body_Executive_i::GetWatchList (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    return new ::Types::ArrayLong(watch_list_);
    // Add your implementation here
  }
  
char * Body_Executive_i::GetStatus (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    return CORBA::string_dup(status_.c_str());
  }
  
void Body_Executive_i::StartCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::map<CORBA::Long, std::string>::iterator iter1;
    std::map<std::string, Body::Unit_var>::iterator iter2;
    for (CORBA::ULong i=0; i<exec_list_.size(); i++)
      {
	iter1=modules_.find(exec_list_[i]);
	if (iter1!=modules_.end())
	  {
	    iter2=units_.find(iter1->second);
	    if (iter2!=units_.end())
	      {
		(iter2->second)->SetID(i);
		(iter2->second)->StartCalc();
	      }
	  }
      }
  }
  
void Body_Executive_i::StopCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::map<CORBA::Long, std::string>::iterator iter1;
    std::map<std::string, Body::Unit_var>::iterator iter2;
    for (CORBA::ULong i=0; i<exec_list_.size(); i++)
      {
	iter1=modules_.find(exec_list_[i]);
	if (iter1!=modules_.end())
	  {
	    iter2=units_.find(iter1->second);
	    if (iter2!=units_.end())
	      (iter2->second)->StopCalc();
	  }
      }
  }
  
void Body_Executive_i::PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::map<CORBA::Long, std::string>::iterator iter1;
    std::map<std::string, Body::Unit_var>::iterator iter2;
    for (CORBA::ULong i=0; i<exec_list_.size(); i++)
      {
	iter1=modules_.find(exec_list_[i]);
	if (iter1!=modules_.end())
	  {
	    iter2=units_.find(iter1->second);
	    if (iter2!=units_.end())
	      (iter2->second)->PauseCalc();
	  }
      }
  }
  
void Body_Executive_i::Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    std::map<CORBA::Long, std::string>::iterator iter1;
    std::map<std::string, Body::Unit_var>::iterator iter2;
    for (CORBA::ULong i=0; i<exec_list_.size(); i++)
      {
	iter1=modules_.find(exec_list_[i]);
	if (iter1!=modules_.end())
	  {
	    iter2=units_.find(iter1->second);
	    if (iter2!=units_.end())
	      (iter2->second)->Resume();
	  }
      }
  }
  
void Body_Executive_i::RegisterUI (
    const char * UIName
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  { 
    std::cout<<"Registering UI"<<std::endl;
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup(UIName);
    
    try {
    CORBA::Object_var ui_object = naming_context_->resolve(name); 
    if (CORBA::is_nil(ui_object))
      std::cout<<"NULL UI_OBJ"<<std::endl;

    std::cout<<"CP 1"<<std::endl;
    
    Body::UI_var ui = Body::UI::_narrow(ui_object.in());

    if (CORBA::is_nil(ui))
      std::cout<<"NULL UI"<<std::endl;
    std::cout<<"CP 2"<<std::endl;
    uis_.insert(std::pair<std::string, Body::UI_var>(std::string(UIName), ui));

    std::cout<<"CP 3"<<std::endl;

    ui->Raise("Hello from exec!");
    std::cout<<UIName<<" is Registrated"<<endl;
    }
    catch (CORBA::Exception &) {
    std::cerr << "CORBA exception raised!" << std::endl;
  }
  return ;
    // Add your implementation here
  }
  
void Body_Executive_i::RegisterUnit (
    const char * UnitName
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    //When this is called, a unit is already binded to the name service, so this call can get it's reference from the name service
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup(UnitName);
    CORBA::Object_var unit_object = naming_context_->resolve(name);
    Body::Unit_var unit = Body::Unit::_narrow(unit_object.in());
 
    units_.insert(std::pair<std::string, Body::Unit_var>(std::string(UnitName), unit));
    
    unit->SetID(1002);
    std::cout<<UnitName<<" is Registrated"<<endl;
    // Add your implementation here
  }
  
