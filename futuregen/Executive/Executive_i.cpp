#include "Executive_i.h"
#include <iostream>


// Implementation skeleton constructor
Body_Executive_i::Body_Executive_i (CosNaming::NamingContext_ptr nc)
  : naming_context_(CosNaming::NamingContext::_duplicate(nc))
  {
    _network = new Network();
    _scheduler = new Scheduler(_network);
  }
  
// Implementation skeleton destructor
Body_Executive_i::~Body_Executive_i (void)
  {
    delete _network;
    //delete _scheduler;
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
    Package p;
    p.SetSysId("temp.xml");
    p.Load(data, strlen(data));
  
    std::vector<Interface>::iterator iter;
    for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
      if(!_network->setPortData(module_id, port_id, &(*iter)))
	cerr << "Unable to set mod id# " << module_id 
	     << ", port id# " << port_id << "'s port data\n";
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
    Interface intf;
    if(!_network->getPortData(module_id, port_id, intf)) {
      cerr << "Unable to get mod id# " << module_id 
	   << ", port id# " << port_id << "'s port data\n";
    }
    
    bool        rv;
    std::string str;

    Package p;
    p.SetSysId("temp.xml");
    p.intfs.push_back(intf);

    str = p.Save(rv);

    if(!rv) return 0;
    
    return CORBA::string_dup(str.c_str());
  }

void Body_Executive_i::SetProfileData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    const Types::Profile & data
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  // Add your implementation here
}

void Body_Executive_i::GetProfileData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    Types::Profile_out data
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  // Add your implementation here
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
  
void Body_Executive_i::SetModuleResult (
    CORBA::Long module_id,
    const char * result
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  // Add your implementation here
}

char * Body_Executive_i::GetModuleResult (
    CORBA::Long module_id
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  // Add your implementation here
  
  return 0;
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
    Package p;
    p.SetSysId("temp.xml");
    p.Load(network, strlen(network));
    
    _network->clear();
    _scheduler->clear();
    
    std::vector<Interface>::iterator iter;
    for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
      if(iter->_id==-1) break;
    
    if(iter!=p.intfs.end() && _network->parse(&(*iter)) && _scheduler->schedule(0)) {
      for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
	if(!_network->setInput(iter->_id, &(*iter)))
	  cerr << "Unable to set id# " << iter->_id << "'s inputs\n";
      
      _scheduler->print_schedule();
    } else {
      cerr << "Error in SetNetwork\n";
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
    return 0;
  }

void Body_Executive_i::SetModuleUI (
    CORBA::Long module_id,
    const char * ui
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
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
    // mod->StartCalc();
  }
  
void Body_Executive_i::StopCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // mod->StopCalc();
  }
    
void Body_Executive_i::PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // mod->PauseCalc();
  }
  
void Body_Executive_i::Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // mod->Resume();
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
        cerr << "NULL UI_OBJ\n";
    
      Body::UI_var ui = Body::UI::_narrow(ui_object.in());

      if (CORBA::is_nil(ui))
        cerr << "NULL UI\n";
      //uis_.insert(std::pair<std::string, Body::UI_var>(std::string(UIName), ui));
	  uis_[std::string(UIName)]=ui;
      ui->Raise("Hello from exec!");
      cerr << UIName << " : registered a UI\n";
    }
    catch (CORBA::Exception &ex) {
      //std::cerr << "CORBA exception raised! : " <<ex._name<< std::endl;
	  //std::cerr << ex._info<<std::endl;
	}
    return ;
  }
  
void Body_Executive_i::RegisterUnit (
    const char * UnitName,
    CORBA::Long flag
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
  
  // mod = Body::Unit::_narrow(unit_object.in());
  // mod->SetID(1002);
  
}
  
void Body_Executive_i::UnRegisterUI (
    const char * UIName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  // Add your implementation here
}

void Body_Executive_i::UnRegisterUnit (
    const char * UnitName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  // Add your implementation here
}

CORBA::Long Body_Executive_i::GetGlobalMod (
    Types::ArrayLong_out ids
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  // Add your implementation here
  
  return CORBA::Long(0);
}
