/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: Executive_i.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/string_ops.h"
#include "VE_CE/Executive_i.h"
 
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
 
  std::cout << "GetImportData " << module_id << " " << port_id << std::endl;

  Module *mod = _network->module(_network->moduleIdx(module_id));
  if(!mod) {
     std::cerr << "Cannot find module, id# " << module_id << std::endl;
    return CORBA::string_dup("");  
  }
  IPort *iport = mod->getIPort(mod->iportIdx(port_id));
  
  bool        rv = false;
  std::string str;
  
  if(iport && iport->nconnections()) {
    Connection* conn=iport->connection(0); // should only have one connection
    OPort* oport=conn->get_oport();
    
    if(oport->have_data()) {
      Package p;
      p.SetSysId("temp.xml");
      p.intfs.push_back(oport->_data);
      
      str = p.Save(rv);
    } else {
      string msg = "Mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+" has no data\n" ;
      std::cerr << msg;
      ClientMessage(msg.c_str());
    }
  } else {
    string msg = "Unable to get mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+"\n" ;
    std::cerr << msg;
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
      std::cerr << "Cannot find execution thread for " << mn << std::endl;
    } 
  else {
    if(!_exec_thread[mn]->needexecute()) 
      {
   msg = "Failed to execute " + mn +"\n";
   std::cerr << msg;
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
  
  std::cout << "SetExportData "<< module_id << " " << port_id << std::endl;

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
  std::cout << "GetExportData "<< module_id << " " << port_id << std::endl;
  
  Interface intf;
  if(!_network->getPortData(module_id, port_id, intf)) {
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

  std::cout << " SetProfileData "<< module_id << " " << port_id << std::endl;
 
  std::string msg;
  
  if(!_network->setPortProfile(module_id, port_id, &data)) {
    msg = "Unable to set mod id# " + to_string(module_id) + ", port id# " + to_string(port_id)+ "'s port profile\n";
    ClientMessage(msg.c_str());
  } else {
    ; //std::cout << "SetPortProfile success\n";
  }
 
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
 
  std::cout << "GetProfileData " << module_id << " " << port_id << std::endl;

  Module *mod = _network->module(_network->moduleIdx(module_id));
  if(!mod) {
    std::cerr << "Cannot find module, id# " << module_id << std::endl;
    return;
  }
  IPort *iport = mod->getIPort(mod->iportIdx(port_id));
  
  if(iport && iport->nconnections()) {
    Connection* conn=iport->connection(0); // should only have one connection
    OPort* oport=conn->get_oport();
    
    if(oport->have_profile()) {
      data = new Types::Profile(*(oport->_profile));
    } else {
      string msg = "Mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+" has no Profile\n" ;
      std::cerr << msg;
      ClientMessage(msg.c_str());
    }
  } else {
    string msg = "Unable to get Profile from mod #" + to_string(module_id) + " IPort, id #" + to_string(port_id)+"\n" ;
    std::cerr << msg;
    ClientMessage(msg.c_str());	
  }
  
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
      std::cerr << "Cannot contact Module " << module_id << std::endl;
      }
    }
    else {
      std::cerr << "Cannot contact module of " << mod_type << " type" << std::endl;
    }
  }
  catch (CORBA::Exception &) {
    std::cerr << "Cannot contact Module " << module_id << std::endl;
  }
  
  Package sm;
  sm.SetSysId("temp.xml");
  sm.Load(msg, strlen(msg));
  
  delete msg;
  
  if(sm.intfs.size()!=0) {
    int rs = sm.intfs[0].getInt("RETURN_STATE"); // 0:O.K, 1:ERROR, 2:?, 3:FB COMLETE
    if(rs==-1) rs = sm.intfs[0].getInt("return_state");
    _network->module(_network->moduleIdx(module_id))->_return_state = rs;
    
    if(rs!=1) {
      int rt = _scheduler->execute(_network->module(_network->moduleIdx(module_id)))-1;
      if(rt<0) {
         ClientMessage("Network execution complete\n");
         
      }
      else if(_mod_units.find(_network->module(rt)->_name)==_mod_units.end()) {
	std::cerr <<  "Cannot find running unit " << _network->module(rt)->_name << std::endl;
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
     
      //std::cout << _network->module(rt)->_name << "\n" << str2 << std::endl;
      
      try {
         _mod_units[_network->module(rt)->_name]->SetParams(str2.c_str());
         _mod_units[_network->module(rt)->_name]->SetID((long)_network->module(rt)->_inputs._id);
         execute(_network->module(rt)->_name);
      }
      catch(CORBA::Exception &) {
      std::cerr << "Cannot contact Module " << module_id << std::endl;
      }
   }
   else {
      std::cerr << "Error packing " << module_id << "'s Inputs" << std::endl;
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
   std::cout << "SetModuleMessage " << msg << std::endl;

   std::map<std::string, Body::UI_var>::iterator iter;
   for(iter=uis_.begin(); iter!=uis_.end(); iter++) 
   {
      std::cout << msg << " :TO: " << iter->first << std::endl;
      try 
      {
         iter->second->Raise(msg);
      }
      catch (CORBA::Exception &) 
      {
         _mutex.acquire();
         std::cout << iter->first <<" is obsolete.\n";
         uis_.erase( iter );
         _mutex.release();
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
      // std::cerr << "Unable to set mod id# " << module_id 
   //    << "'s Message data" << std::endl;
/*  
  //std::vector<Interface>::iterator iter2;
  //for(iter2=p.intfs.begin(); iter2!=p.intfs.end(); iter2++)
  //  if(!_network->setMessage(module_id, &(*iter2)))
  //    std::cerr << "Unable to set mdo id# " << module_id
   //    << "'s Message data << std::endl";
*/
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
      // Keep track of power requirements
      bool f = false;
      std::string p = iter->getString(std::string("Power (MW)"), &f);
      if(f) _module_powers[module_id] = atof(p.c_str());
      std::string ti = iter->getString(std::string("Thermal Input (MW)"), &f);
      if(f) _thermal_input[module_id] = atof(ti.c_str()); //changed by yang
      //original code is  //if(f) _thermal_input = atof(ti.c_str());
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
  
   if ( module_id == -1 ) 
   {
      // Calculate efficiency
      double tot_power = 0.0;
      double tot_thermo = 0.0;

      std::map<long, double>::iterator iter;
      for(iter=_module_powers.begin(); iter!=_module_powers.end(); iter++)
         tot_power += iter->second;
      
      for(iter=_thermal_input.begin(); iter!=_thermal_input.end(); iter++)
         tot_thermo += iter->second;
      
      intf.setString("Power (MW)", to_string(tot_power));
      
      if(tot_thermo != 0.0) 
      {
         intf.setString("Thermal Input (MW)", to_string(tot_thermo));
         intf.setString("Efficiency (%)", to_string(tot_power / tot_thermo * 100));
      }
   }
   else if ( !_network->getOutput(module_id, intf) ) 
   {
      
      std::cerr << "Unable to get mod id# " << module_id << "'s ouput data" << std::endl;
   }
  
   bool        rv;
   std::string str;
  
   Package p;
   p.SetSysId("temp.xml");
   p.intfs.push_back(intf);
  
   str = p.Save(rv);
   _mutex.release();

   if ( rv ) 
      return CORBA::string_dup(str.c_str());

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
  
   // Keep track of power requirements
   _module_powers.clear();
   _thermal_input.clear();

   std::vector<Interface>::iterator iter;
   for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
      if(iter->_id==-1) 
         break;

   // This if statement does not take into account global
   // data. In this statement the global data that that 
   // is sent gets stripped out because there is no 
   // mechanism to handle it yet.
   if ( iter!=p.intfs.end() && _network->parse(&(*iter)) ) 
   {
      _network_intf = *iter; //_network_intf is the first block of the network xml.
      for(iter=p.intfs.begin(); iter!=p.intfs.end(); iter++)
      {
         //if ( iter->_type == 1 ) // this block is for inputs not geom
         {
            if(iter->_category==1 && iter->_type == 1 &&_network->setInput(iter->_id, &(*iter))) 
            {
	            _network->module(_network->moduleIdx(iter->_id))->_is_feedback  = iter->getInt("FEEDBACK");
               _network->module(_network->moduleIdx(iter->_id))->_need_execute = 1;
               _network->module(_network->moduleIdx(iter->_id))->_return_state = 0;
               _network->module(_network->moduleIdx(iter->_id))->_type = iter->_type;
               _network->module(_network->moduleIdx(iter->_id))->_category = iter->_category;
            }
            else if(iter->_category==1 && iter->_type == 2 &&_network->setGeomInput(iter->_id, &(*iter)))
            {
               std::cout<<"The current module has geom info "<<std::endl;
            }
            else
            {
               std::cerr << "Unable to set id# " << iter->_id << "'s inputs" << std::endl;
            }
         }
      }

      _mutex.release();
      if(!_scheduler->schedule(0))
      {
         ClientMessage("Error in Schedule\n");
         return;
      }
      else 
      {
         ClientMessage("Successfully Scheduled Network\n");
         _scheduler->print_schedule();
      }
   } 
   else 
   {
      _mutex.release();
      ClientMessage("Error in SetNetwork\n");
      return;
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
  _mutex.acquire();
  
  int         i;
  bool        rv;
  std::string str;
  
  Package p;
  p.SetSysId("temp.xml");
  p.intfs.push_back(_network_intf);
 
  for(i=0; i<_network->nmodules(); i++)
  {
     p.intfs.push_back(_network->module(i)->_inputs);

     if(_network->module(i)->hasGeomInfo())
     {
         p.intfs.push_back(_network->module(i)->_geominputs);
     }
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
   {
      //if ( iter->_type == 1 ) // this block is for inputs not geom
      {
         if(_network->setInput(module_id, &(*iter))) 
         {
            _network->module(_network->moduleIdx(iter->_id))->_need_execute = 1;
            _network->module(_network->moduleIdx(iter->_id))->_return_state = 0;
         }
         else
            std::cerr << "Unable to set mod id# " << module_id << "'s Input data" << std::endl;
      }
   }
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
   //_scheduler->reset();

   if(_scheduler->snodes_size()==0) return;

   _mutex.acquire();
  
   int rt = _scheduler->execute(0)-1;
   int module_id = _network->module(rt)->_inputs._id;
  
   if(rt<0) 
   {
      std::cerr << "Network execution complete" << std::endl;
   }
   else 
   {
      bool        rv2;
      std::string str2;
    
      Package p2;
      p2.SetSysId("temp.xml");
      //p2.intfs.push_back(_network->module(_network->moduleIdx(module_id))->_inputs);
      p2.intfs.push_back(_network->module(rt)->_inputs);
      if(_network->module(rt)->hasGeomInfo())
      {
         p2.intfs.push_back(_network->module(rt)->_geominputs);
      }
      str2 = p2.Save(rv2);
    
      if(rv2) 
      {
         //std::cout << _network->module(rt)->_name << "\n" << str2 << std::endl;
         
          try 
          {
            std::cerr << "Initial Execute" << std::endl;
            if ( !_mod_units.empty() )
            {
               if(_mod_units.find(_network->module(rt)->_name)!=_mod_units.end())
               {
                  _mod_units[_network->module(rt)->_name]->SetParams(str2.c_str());
                  _mod_units[_network->module(rt)->_name]->SetID((long)_network->module(rt)->_inputs._id);
                  execute(_network->module(rt)->_name);
               }
               else
               {
                  std::cerr << "Initial Execute, cannot contact Module " << _network->module(rt)->_name << std::endl;
               }
            }
            else
            {
               std::cerr << " No Module Units connected to the VE-CE, skipping execution " << std::endl;
            }
         }
         catch(CORBA::Exception &) 
         {
            std::cerr << "Initial Execute, cannot contact Module " << module_id << std::endl;
         }
      }
      else 
      {
         std::cerr << "Initial Execute, error packing " << module_id << "'s Inputs" << std::endl;
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
    const char * UIName,
      Body::UI_ptr ui_ptr
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{ 
   _mutex.acquire();
  
   //CosNaming::Name name(1);
   //name.length(1);
   //name[0].id = CORBA::string_dup(UIName);
  
   try 
   {
      //  CORBA::Object_var ui_object = naming_context_->resolve(name); 
      //  if (CORBA::is_nil(ui_object))
      //   std::cerr << "NULL UI_OBJ" << std::endl;
    
      //Body::UI_var ui = Body::UI::_narrow(ui_object.in());
    
      Body::UI_var ui = Body::UI::_duplicate(ui_ptr);
      if (CORBA::is_nil(ui))
         std::cerr << "NULL UI" << std::endl;

      //uis_.insert(std::pair<std::string, Body::UI_var>(std::string(UIName), ui));
      uis_[std::string(UIName)] = ui;

      _mutex.release();
      ui->Raise("Connected to Executive\n");
      std::cerr << UIName << " : registered a UI" << std::endl;
   }
   catch (CORBA::Exception &ex) 
   {
      //std::cerr << "CORBA exception raised! : " <<ex._name<< std::endl;
      //std::cerr << ex._info<<std::endl;
      std::cerr<<"Can't call be UI "<<UIName<< std::endl;
      _mutex.release();
   }
   return;
}

void Body_Executive_i::RegisterUnit (
    const char * UnitName,
   Body::Unit_ptr unit,
    CORBA::Long flag
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
{
  //_mutex.acquire();

  static long unit_id;
  std::map<std::string, Execute_Thread*>::iterator iter;
  // When this is called, a unit is already binded to the name service, 
  // so this call can get it's reference from the name service
  std::cerr << "Going to RegisterUnit " << UnitName << std::endl;
  //CosNaming::Name name(1);
  //name.length(1);
  //name[0].id = CORBA::string_dup(UnitName);
  //CORBA::Object_var unit_object = naming_context_->resolve(name);
  
  //std::cerr << "RegisterUnit " << UnitName << std::endl;

  _mod_units[std::string(UnitName)] = Body::Unit::_duplicate(unit);
  _mod_units[std::string(UnitName)]->SetID(unit_id++);
  
  if((iter=_exec_thread.find(std::string(UnitName)))==_exec_thread.end()) {
    // CLEAN THIS UP IN UNREGISTER UNIT !
    Execute_Thread *ex = new Execute_Thread(_mod_units[std::string(UnitName)], (Body_Executive_i*)this);
    ex->activate();

    _exec_thread[std::string(UnitName)] = ex;
  }
  else //replace it with new reference
  {

   ACE_Task_Base::cleanup(iter->second, NULL);
   if (iter->second)
      delete iter->second;
   Execute_Thread *ex = new Execute_Thread(_mod_units[std::string(UnitName)], (Body_Executive_i*)this);
    ex->activate();
   iter->second=ex;
  }

  //_mutex.release();
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
      std::cout<<UIName<<" Unregistered!\n";
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
  std::map<std::string, Execute_Thread*>::iterator iter;
  iter=_exec_thread.find(std::string(UnitName));
  if (iter!=_exec_thread.end())
  {
	ACE_Task_Base::cleanup(iter->second, NULL);
	if (iter->second)
		delete iter->second;

	_exec_thread.erase(iter);
  }
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
      std::cout << msg << " :TO: " << iter->first << std::endl;
	   try 
      {
		   iter->second->Raise(msg);
	   }
      catch (CORBA::Exception &) 
      {
         std::cout <<iter->first<<" is obsolete.\n";
		   uis_.erase(iter);
	   }
   }
}
