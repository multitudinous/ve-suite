#include "Executive_i.h"
#include <iostream>


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
    Interface intf;

    intf.unpack_ids(&data[0]);
    intf.unpack(&data[96]);

    port_data_[intf._id] = intf;
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
    if (port_data_.find(module_id)!=port_data_.end()) {
      std::string packed;
      port_data_[module_id].pack(packed);
      return CORBA::string_dup(packed.c_str());
    }
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
    if (msg_data_.find(module_id)!=msg_data_.end())
      msg_data_[module_id].msg=std::string(msg);
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
    char buf[25], *buf2;
    unsigned int size, netlength = strlen(network), ipos = 0;
    
    network_.clear();

    // Unpack incoming network string into individual interfaces,
    // and place them into the network_ structure.

    while(1) {
       
      Interface intf;
      intf.unpack_ids(&network[ipos]);
	 
      network_[intf._id] = intf;

      ipos+=72;
     
      strncpy(buf, &network[ipos], 24);
      ipos+=24;
      buf[24]='\0';
      size = atoi(buf);
      
      buf2 = new char[size+1];
      strncpy(buf2, &network[ipos], size);
      ipos+=size;
      network_[intf._id].unpack(buf2);
      delete [] buf2;
           
      if(ipos>=netlength) break;
    }
    
    // Give iterfaces a name
    std::map<int, Interface>::iterator iter;
    
    for(iter=network_.begin(); iter!=network_.end(); iter++) {
      if(iter->first>=0 && iter->first<99999) {
	    char modcl[50];
	    sprintf(modcl, "modCls_%.4d", (iter->first));
	    (iter->second).setString("NAME_", network_[-1].getString(modcl));
      }
      else if(iter->first==100000)
		  (iter->second).setString("NAME_", "GLOBAL");
	  else if(iter->first==-1)
        (iter->second).setString("NAME_", "NETWORK");
    }

	std::map<std::string, Body::UI_var>::iterator iter2;
	
	for(iter2=uis_.begin(); iter2!=uis_.end(); iter2++) {
	  std::cout << (std::string) (iter2->first) <<"Raise calling " << std::endl;
		try {
         (*iter2).second->Raise("");
		 std::cout << (std::string) (iter2->first) <<"Raise called " << std::endl;
	  }
	  catch (CORBA::Exception &ex) {
  		  std::cerr <<"uis_.size before erase "<<uis_.size()<<endl;
		  std::cerr << (std::string) (iter2->first) <<"Raise call failed" << std::endl;
		  uis_.erase(iter2); 
		  std::cerr<<"uis_.size after erase "<<uis_.size()<<endl;
		  iter2=uis_.begin();
	  }

	}

  }
  
char * Body_Executive_i::GetNetwork ( 
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    std::string network, packed;

    std::map<int, Interface>::iterator iter;
    for(iter=network_.begin(); iter!=network_.end(); iter++) {
      (iter->second).pack(packed);
      network += packed;
    }
      
     return CORBA::string_dup(network.c_str());
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
    std::string packed;

    std::map<int, Interface>::iterator iter;
    for(iter=network_.begin(); iter!=network_.end(); iter++) {
     (iter->second).pack(packed);
       cmu_unit_->SetParams(packed.c_str());
    }

    cmu_unit_->StartCalc();

	std::map<std::string, Body::UI_var>::iterator iter2;
	
	for(iter2=uis_.begin(); iter2!=uis_.end(); iter2++) {
	  std::cout << (std::string) (iter2->first) <<"Raise calling " << std::endl;
		try {
	
         (*iter2).second->Raise("");
		 std::cout << (std::string) (iter2->first) <<"Raise called " << std::endl;
	  }
	  catch (CORBA::Exception &ex) {
		  std::cerr <<"uis_.size before erase "<<uis_.size()<<endl;
		  std::cerr << (std::string) (iter2->first) <<"Raise call failed" << std::endl;
		  uis_.erase(iter2); 
		  std::cerr<<"uis_.size after erase "<<uis_.size()<<endl;
		  iter2=uis_.begin();
	
		  
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
    cmu_unit_->StopCalc();
  }
    
void Body_Executive_i::PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
   cmu_unit_->PauseCalc();
  }
  
void Body_Executive_i::Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    cmu_unit_->Resume();
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
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup(UIName);
    
    try {
      CORBA::Object_var ui_object = naming_context_->resolve(name); 
      if (CORBA::is_nil(ui_object))
        std::cout<<"NULL UI_OBJ"<<std::endl;
    
      Body::UI_var ui = Body::UI::_narrow(ui_object.in());

      if (CORBA::is_nil(ui))
        std::cout<<"NULL UI"<<std::endl;
      //uis_.insert(std::pair<std::string, Body::UI_var>(std::string(UIName), ui));
	  uis_[std::string(UIName)]=ui;
      ui->Raise("Hello from exec!");

	  std::cout <<UIName<< " : registered a UI\n";
    }
    catch (CORBA::Exception &ex) {
      //std::cerr << "CORBA exception raised! : " <<ex._name<< std::endl;
	  //std::cerr << ex._info<<std::endl;
	}
    return ;
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
    // When this is called, a unit is already binded to the name service, 
    // so this call can get it's reference from the name service
    CosNaming::Name name(1);
    name.length(1);
    name[0].id = CORBA::string_dup(UnitName);
    CORBA::Object_var unit_object = naming_context_->resolve(name);
    cmu_unit_ = Body::Unit::_narrow(unit_object.in());
     
    cmu_unit_->SetID(1002);
    // Add your implementation here
  }
  
