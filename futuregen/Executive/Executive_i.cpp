#include "string_ops.h"
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
  delete _scheduler;
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
  _mutex.acquire();
 
  cout << "GetImportData " << module_id << " " << port_id << endl;

  Module *mod = _network->module(_network->moduleIdx(module_id));
  IPort *iport = mod->getIPort(mod->iportIdx(port_id));
  
  bool        rv = false;
  std::string str;
  
  if(iport && iport->nconnections()) {
    Connection* conn=iport->connection(0); // should only have one connection
    OPort* oport=conn->get_oport();
    
    Package p;
    p.SetSysId("temp.xml");
    p.intfs.push_back(oport->_data);
    
    str = p.Save(rv);
  } else {
	string msg = "Unable to get mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+"\n" ;
    cerr << msg;
    ClientMessage(msg.c_str());
	
  }
  
  _mutex.release();
  
  if(rv) return CORBA::string_dup(str.c_str());
  
  return CORBA::string_dup("");//str.c_str());//"yang";//0;
}

void Body_Executive_i::execute (std::string mn)
{
  string msg;
  if(_exec_thread.find(mn)==_exec_thread.end()) 
  {
    cerr << "Cannot find execution thread for " << mn << endl;
  } 
  else {
    if(!_exec_thread[mn]->needexecute()) 
	{
		msg = "Failed to execute " + mn +"\n";
		cerr << msg;
		ClientMessage(msg.c_str());
	}
    else 
	{
		msg = "Executing " + mn +"\n";
		ClientMessage(msg.c_str());
	}
  }
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
	string msg;
  _mutex.acquire();
  
  cout << "SetExportData\n";

  Package p;
  p.SetSysId("temp.xml");
  p.Load(data, strlen(data));
  
  // Should only be one item. But, maybe later...
  std::vector<Interface>::iterator iter;
  for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
    if(!_network->setPortData(module_id, port_id, &(*iter)))
	{
		msg = "Unable to set mod id# " + to_string(module_id) + ", port id# " + to_string(port_id)+ "'s port data\n";
		ClientMessage(msg.c_str());
	}
  _mutex.release();
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
  _mutex.acquire();
  string msg;
  cout << "GetExportData\n";
  
  Interface intf;
  if(!_network->getPortData(module_id, port_id, intf)) 
	{
		msg = "Unable to get mod id# " + to_string(module_id) + ", port id# " + to_string(port_id)+ "'s port data\n";
		ClientMessage(msg.c_str());
	}
    
    
  bool        rv;
  std::string str;
  
  Package p;
  p.SetSysId("temp.xml");
  p.intfs.push_back(intf);
  
  str = p.Save(rv);
  
  _mutex.release();
  
  if(rv) return CORBA::string_dup(str.c_str());

  return CORBA::string_dup("");//str.c_str());//"yang";//0;
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
  _mutex.acquire();
 
  // Stuff here

  _mutex.release();
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
  _mutex.acquire();

  // Stuff here

  _mutex.release();
}

void Body_Executive_i::execute_next_mod (long module_id)
{
  char *msg;
  
  try {
    std::string mod_type = _network->module(_network->moduleIdx(module_id))->_name;
    if(_mod_units.find(mod_type)!=_mod_units.end()) {
      try {
	msg = _mod_units[mod_type]->GetStatusMessage();
      }
      catch(CORBA::Exception &) {
	cerr << "Cannot contact Module " << module_id << endl;
      }
    }
    else {
      cerr << "Cannot contact module of " << mod_type << " type\n";
    }
  }
  catch (CORBA::Exception &) {
    cerr << "Cannot contact Module " << module_id << endl;
  }
  
  Package sm;
  sm.SetSysId("temp.xml");
  sm.Load(msg, strlen(msg));
  
  delete msg;
  
  if(sm.intfs.size()!=0) {
    int rs = sm.intfs[0].getInt("RETURN_STATE"); // 0:O.K, 1:ERROR, 2:?, 3:FB COMLETE
    _network->module(_network->moduleIdx(module_id))->_return_state = rs;
    
    if(rs!=1) {
      int rt = _scheduler->execute(_network->module(_network->moduleIdx(module_id)))-1;
      if(rt<0) {
	ClientMessage("Network execution complete\n");
	
      }
      else if(_mod_units.find(_network->module(rt)->_name)==_mod_units.end()) {
	cerr <<  "Cannot find running unit " << _network->module(rt)->_name << endl;
      }
      else {
	
	bool        rv2;
	std::string str2;
	
	Package p2;
	p2.SetSysId("temp.xml");
	// p2.intfs.push_back(_network->module(_network->moduleIdx(module_id))->_inputs);
	p2.intfs.push_back(_network->module(rt)->_inputs);
	str2 = p2.Save(rv2);
	
	if(rv2) {
	  
	  //cout << _network->module(rt)->_name << "\n" << str2 << endl;
	  
	  try {
	    _mod_units[_network->module(rt)->_name]->SetParams(str2.c_str());
	    _mod_units[_network->module(rt)->_name]->SetID((long)_network->module(rt)->_inputs._id);
	    execute(_network->module(rt)->_name);
	  }
	  catch(CORBA::Exception &) {
	    cerr << "Cannot contact Module " << module_id << endl;
	  }
	}
	else {
	  cerr << "Error packing " << module_id << "'s Inputs\n";
	}
      }
    }
  }
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
  _mutex.acquire();
  
  cout << "SetModuleMessage " << msg << endl;

  std::map<std::string, Body::UI_var>::iterator iter;
  for(iter=uis_.begin(); iter!=uis_.end(); iter++) {
    cout << msg << " :TO: " << iter->first << endl;
	try {
		iter->second->Raise(msg);
	}catch (CORBA::Exception &) {
		
		cout <<iter->first<<" is obsolete.\n";
		uis_.erase(iter);
	}
  }

  // THIS EXPECTS AN INTERFACE - NOT A STRING

  // Package p;
  // p.SetSysId("temp.xml");
  // p.Load(msg, strlen(msg));
  
  // Should only be one item. But, maybe later...
  // std::vector<Interface>::iterator iter;
  // for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
    // if(!_network->setMessage(module_id, &(*iter)))
      // cerr << "Unable to set mod id# " << module_id 
	//    << "'s Message data\n";
  
  std::vector<Interface>::iterator iter;
  for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
    if(!_network->setMessage(module_id, &(*iter)))
      cerr << "Unable to set mdo id# " << module_id
	   << "'s Message data\n";

  _mutex.release();
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
  _mutex.acquire();

  string msg;
  Package p;
  p.SetSysId("temp.xml");
  p.Load(result, strlen(result));

  // Should only be one item. But, maybe later...
  std::vector<Interface>::iterator iter;
  for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
    if(_network->setOutput(module_id, &(*iter))) {
      ; // setOutput O.K.
    } else {
		msg = "Unable to set mod id# " + to_string(module_id) + "'s Output data\n";
		ClientMessage(msg.c_str());
    }

	msg = "Mod id# "+ to_string(module_id) + "'s Execution is done\n";
	ClientMessage(msg.c_str());

  _mutex.release();
}

char * Body_Executive_i::GetModuleResult (
    CORBA::Long module_id
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  Interface intf;
  if(!_network->getOutput(module_id, intf)) {
	  
    cerr << "Unable to get mod id# " << module_id 
	 << "'s ouput data\n";
  }
  
  bool        rv;
  std::string str;
  
  Package p;
  p.SetSysId("temp.xml");
  p.intfs.push_back(intf);
  
  str = p.Save(rv);
  

  _mutex.release();

  if(rv) return CORBA::string_dup(str.c_str());

  return CORBA::string_dup("");//str.c_str());//"yang";//0;
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
  _mutex.acquire();
  
  Package p;
  p.SetSysId("temp.xml");
  p.Load(network, strlen(network));
  
  _network->clear();
  _scheduler->clear();
  
  std::vector<Interface>::iterator iter;
  for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
    if(iter->_id==-1) break;
  
  if(iter!=p.intfs.end() && _network->parse(&(*iter))) {
    _network_intf = *iter;
    for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
      if(_network->setInput(iter->_id, &(*iter))) {
	_network->module(_network->moduleIdx(iter->_id))->_is_feedback  = iter->getInt("FEEDBACK");
	_network->module(_network->moduleIdx(iter->_id))->_need_execute = 1;
	_network->module(_network->moduleIdx(iter->_id))->_return_state = 0;
      }
      else
	cerr << "Unable to set id# " << iter->_id << "'s inputs\n";
    if(!_scheduler->schedule(0))
      ClientMessage("Error in Schedule\n");
    else {
      ClientMessage("Successfully Scheduled Network\n");
      _scheduler->print_schedule();
    }
  } else {
    ClientMessage("Error in SetNetwork\n");
  }
  
  _mutex.release();
}
  
char * Body_Executive_i::GetNetwork ( 
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  int         i;
  bool        rv;
  std::string str;
  
  Package p;
  p.SetSysId("temp.xml");
  p.intfs.push_back(_network_intf);
  
  for(i=0; i<_network->nmodules(); i++) {
    p.intfs.push_back(_network->module(i)->_inputs);
  }
  
  str = p.Save(rv);
   
  _mutex.release();
  
  if(rv) return CORBA::string_dup(str.c_str());

  return CORBA::string_dup("");//str.c_str());//"yang";//0;
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
  _mutex.acquire();
  
  Package p;
  p.SetSysId("temp.xml");
  p.Load(ui, strlen(ui));
  
  // Should only be one item. But, maybe later...
  std::vector<Interface>::iterator iter;
  for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
    if(_network->setInput(module_id, &(*iter))) {
      _network->module(_network->moduleIdx(iter->_id))->_need_execute = 1;
      _network->module(_network->moduleIdx(iter->_id))->_return_state = 0;
    }
    else
      cerr << "Unable to set mod id# " << module_id 
	   << "'s Input data\n";
  
  _mutex.release();
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
  _mutex.acquire();
  
  watch_list_ = id;
  
  _mutex.release();
}

::Types::ArrayLong * Body_Executive_i::GetWatchList (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // Stuff here

  _mutex.release();
  
  return new ::Types::ArrayLong(watch_list_);
}

char * Body_Executive_i::GetStatus (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // Stuff here
  
  _mutex.release();
  
  return CORBA::string_dup("");//str.c_str());//"yang";//0;
}
  
void Body_Executive_i::StartCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  if(_scheduler->snodes_size()==0) return;

  _mutex.acquire();
  
  int rt = _scheduler->execute(0)-1;
  int module_id = _network->module(rt)->_inputs._id;
  
  if(rt<0) {
    cerr << "Network execution complete\n";
  }
  else {
    bool        rv2;
    std::string str2;
    
    Package p2;
    p2.SetSysId("temp.xml");
    //p2.intfs.push_back(_network->module(_network->moduleIdx(module_id))->_inputs);
    p2.intfs.push_back(_network->module(rt)->_inputs);
    str2 = p2.Save(rv2);
    
    if(rv2) {

      //cout << _network->module(rt)->_name << "\n" << str2 << endl;
      
      try {
	cerr << "Initial Execute\n";
	_mod_units[_network->module(rt)->_name]->SetParams(str2.c_str());
	_mod_units[_network->module(rt)->_name]->SetID((long)_network->module(rt)->_inputs._id);
	execute(_network->module(rt)->_name);
      }
      catch(CORBA::Exception &) {
	cerr << "Initial Execute, cannot contact Module " << module_id << endl;
      }
    }
    else {
      cerr << "Initial Execute, error packing " << module_id << "'s Inputs\n";
    }
  }
  
  _mutex.release();
}
  
void Body_Executive_i::StopCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // mod->StopCalc();
  
  _mutex.release();
}

void Body_Executive_i::PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // mod->PauseCalc();
  
  _mutex.release();
}

void Body_Executive_i::Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // mod->Resume();
  
  _mutex.release();
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
  _mutex.acquire();
  
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
    uis_[std::string(UIName)] = ui;
    ui->Raise("Connected to Executive\n");
    cerr << UIName << " : registered a UI\n";
  }
  catch (CORBA::Exception &ex) {
    //std::cerr << "CORBA exception raised! : " <<ex._name<< std::endl;
    //std::cerr << ex._info<<std::endl;
	  std::cerr<<"Can't call be UI "<<UIName<<"\n";
  }
  
  _mutex.release();
  
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
  _mutex.acquire();
  
  static long unit_id;
  
  // When this is called, a unit is already binded to the name service, 
  // so this call can get it's reference from the name service
  CosNaming::Name name(1);
  name.length(1);
  name[0].id = CORBA::string_dup(UnitName);
  CORBA::Object_var unit_object = naming_context_->resolve(name);
  
  cerr << "RegisterUnit " << UnitName << endl;

  _mod_units[std::string(UnitName)] = Body::Unit::_narrow(unit_object.in());
  _mod_units[std::string(UnitName)]->SetID(unit_id++);
  
  if(_exec_thread.find(std::string(UnitName))==_exec_thread.end()) {
    // CLEAN THIS UP IN UNREGISTER UNIT !
    Execute_Thread *ex = new Execute_Thread(_mod_units[std::string(UnitName)], (Body_Executive_i*)this);
    ex->activate();
    _exec_thread[std::string(UnitName)] = ex;
  }

  _mutex.release();
}
  
void Body_Executive_i::UnRegisterUI (
    const char * UIName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
	std::map<std::string, Body::UI_var>::iterator iter;
	_mutex.acquire();

  
	// Add your implementation here
	iter = uis_.find(std::string(UIName));
	if (iter!= uis_.end())
	{
		uis_.erase(iter);
		cout<<UIName<<" Unregistered!\n";
	}
  _mutex.release();
}

void Body_Executive_i::UnRegisterUnit (
    const char * UnitName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // Add your implementation here
  
  _mutex.release();
}

CORBA::Long Body_Executive_i::GetGlobalMod (
    Types::ArrayLong_out ids
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  _mutex.acquire();
  
  // Add your implementation here
  
  _mutex.release();
  
  return CORBA::Long(0);
}

void Body_Executive_i::ClientMessage(const char *msg)
{
	std::map<std::string, Body::UI_var>::iterator iter;
	for(iter=uis_.begin(); iter!=uis_.end(); iter++) 
	{
		cout << msg << " :TO: " << iter->first << endl;
	try {
		iter->second->Raise(msg);
	}catch (CORBA::Exception &) {
		
		cout <<iter->first<<" is obsolete.\n";
		uis_.erase(iter);
	}
 }
}
