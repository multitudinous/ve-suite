#include <iostream>
#include "Network_Exec.h"
#include <set>
#include <string>
////////////////////////////////////////////////////////////////////////////////
using namespace std;
Network::Network ()
{
}

Network::~Network ()
{
  clear();
}

void Network::clear ()
{
  unsigned int i;
  for(i=0; i<_module_ptrs.size(); i++) delete _module_ptrs[i];
  for(i=0; i<_connections.size(); i++) delete _connections[i];
  _module_ptrs.clear();
  _connections.clear();
}

int Network::parse (Interface* intf)
{
  int num;
  long int temp;
  unsigned int i, pos;

  std::string temps;
  std::vector<std::string> vars;

  vars = intf->getStrings();
  for (i=0; i<vars.size(); i++) {
    intf->getVal(vars[i], temps);
    if ((pos=vars[i].find("modCls_"))!=string::npos) {
      num =atoi(vars[i].substr(pos+7, 4).c_str());
      add_module(num, temps);
    }
  }

  std::set<int> links;
  std::map<int, int> FrMod, ToMod, FrPort, ToPort;

  vars = intf->getInts();
  for (i=0; i<vars.size(); i++) {
    
    intf->getVal(vars[i], temp);
    
    if ((pos=vars[i].find("ln_FrMod_"))!=string::npos) {
      num = atoi(vars[i].substr(pos+9, 4).c_str());
      FrMod[num] = temp;
      links.insert(num);
    }
    else if ((pos=vars[i].find("ln_ToMod_"))!=string::npos) {
      num = atoi(vars[i].substr(pos+9, 4).c_str());
      ToMod[num] = temp;
      links.insert(num);
    }
    else if ((pos=vars[i].find("ln_FrPort_"))!=string::npos) {
      num = atoi(vars[i].substr(pos+10, 4).c_str());
      FrPort[num] = temp;; 
      links.insert(num);
    }
    else if ((pos=vars[i].find("ln_ToPort_"))!=string::npos) {
      num = atoi(vars[i].substr(pos+10, 4).c_str());
      ToPort[num] = temp;
      links.insert(num);
    }
  }

  if(intf->getInt("Module_size") != (int)_module_ptrs.size()) {
    cerr << "Inconsistent Modules In Network\n";
    return 0;
  }
  if(intf->getInt("Link_size")   != (int)links.size()) {
    cerr << "Inconsistent Links In Network\n";
    return 0;
  }

  std::set<int>::iterator iter;
  for(iter=links.begin(); iter!=links.end(); iter++) {

    if(FrMod.find(*iter)==FrMod.end()  || ToMod.find(*iter)==FrMod.end()   ||
       FrPort.find(*iter)==FrMod.end() || ToPort.find(*iter)==FrMod.end()) {
      cerr << "Bad link found\n";
      return  0;
    }
    
    Connection* cn = new Connection(*iter);
    _connections.push_back(cn);

    if(!addIPort(ToMod[*iter], ToPort[*iter], cn) ||
       !addOPort(FrMod[*iter], FrPort[*iter], cn)) {
      cerr << "Error adding ports\n";
      return 0;
    }
  }
  
  return 1;
}

int Network::addIPort (int m, int p, Connection* c)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->addIPort(p, c);
  return 1;
}

int Network::addOPort (int m, int p, Connection* c)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->addOPort(p, c);
  return 1;
}

int Network::nmodules ()
{
  return _module_ptrs.size();
}

int Network::module (Module* mod)
{
  int i, fi = -1;

  for(i=0; i<(int)_module_ptrs.size(); i++)
    if(mod->get_id()==_module_ptrs[i]->get_id()) fi = i;

  return fi;
}

Module* Network::module (int idx)
{
  if(idx<0 || idx>=(int)_module_ptrs.size()) return NULL;
  return _module_ptrs[idx];
}

int Network::moduleIdx (int id)
{
  int i, fi = -1;

  for(i=0; i<(int)_module_ptrs.size(); i++)
    if(id==_module_ptrs[i]->get_id()) fi = i;

  return fi;
}

void Network::add_module (int m, std::string name)
{
  int fi = moduleIdx(m);

  if(fi>=0) return;
  
  Module *mod = new Module(m, this);
  mod->_name = name;
  _module_ptrs.push_back(mod);
}  

int Network::getInput (int m, Interface& intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_inputs);
  return 1;
}

int Network::setInput (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_inputs.copy(*intf);
  return 1;
}


int Network::getOutput (int m, Interface &intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_outputs);
  return 1;
}

int Network::setOutput (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_outputs.copy(*intf);
  return 1;
}

int Network::getMessage (int m, Interface &intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  intf.copy(_module_ptrs[fi]->_messages);
  return 1;
}

int Network::setMessage (int m, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  _module_ptrs[fi]->_messages.copy(*intf);
  return 1;
}

int Network::getPortData (int m, int p, Interface& intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->getPortData(p, intf);
}

int Network::setPortData (int m, int p, Interface* intf)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->setPortData(p, intf);
}

int Network::getPortProfile (int m, int p, Types::Profile_out& prof)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->getPortProfile(p, prof);
}

int Network::setPortProfile (int m, int p, const Types::Profile* prof)
{
  int fi = moduleIdx(m);
  if(fi<0) return 0;
  return _module_ptrs[fi]->setPortProfile(p, prof);
}

////////////////////////////////////////////////////////////////////////////////

Module::Module (int id, Network* net)
  : _need_execute (true),
    _return_state (0),
    _is_feedback  (0),
    _type         (-1),
    _category     (-1),
    _net          (net),
    _id           (id)
{
}

Module::Module (const Module &m)
{
  copy(m);
}

Module::~Module ()
{
  unsigned int i;
  for(i=0; i<_iports.size(); i++) delete _iports[i];
}

void Module::copy (const Module &m)
{
  if(this==&m) return;

  _need_execute = m._need_execute;
  _inputs       = m._inputs;
  _outputs      = m._outputs;
  _iports       = m._iports;
  _oports       = m._oports;
  _net          = m._net;
  _id           = m._id;
  _type         = m._type;
  _category     = m._category;
}

int Module::numOPorts ()
{
  return (int)_oports.size();
}

int Module::numIPorts ()
{
  return (int)_iports.size();
}

OPort* Module::getOPort  (int idx)
{
  if(idx>=(int)_oports.size() || idx<0) return NULL;
  return _oports[idx];
}

IPort* Module::getIPort (int idx)
{
  if(idx>=(int)_iports.size() || idx<0) return NULL;
  return _iports[idx];  
}

IPort* Module::getFBPort ()
{
  int i, fi = -1;

  for(i=0; i<(int)_iports.size(); i++)
    if(_iports[i]->get_id()==1) fi = i;

  if(fi<0) return NULL;
  return _iports[fi];  
}

int Module::get_id ()
{
  return _id;
}

int Module::iportIdx (int idx)
{
  int i, fi = -1;

  for(i=0; i<(int)_iports.size(); i++)
    if(_iports[i]->get_id()==idx) fi = i;

  return fi;
}

int Module::oportIdx (int idx)
{
  int i, fi = -1;

  for(i=0; i<(int)_oports.size(); i++)
    if(_oports[i]->get_id()==idx) fi = i;

  return fi;
}

void Module::addIPort (int p, Connection* c)
{
  int sz =  (int)_iports.size(), fi = iportIdx(p);
  
  if(fi<0) {
    fi = sz;
    IPort *ip = new IPort(p, this);
    _iports.push_back(ip);
  }

  _iports[fi]->add_connection(c);
  c->connect_iport(_iports[fi]);
}

void Module::addOPort (int p, Connection* c)
{
  int sz = (int)_oports.size(), fi = oportIdx(p);

  if(fi<0) {
    fi = sz;
    OPort* op = new OPort(p, this);
    _oports.push_back(op);
  }

  _oports[fi]->add_connection(c);
  c->connect_oport(_oports[fi]);
}

int Module::getPortData (int p, Interface& intf)
{
  int fi = oportIdx(p);
  if(fi<0) return 0;
  intf.copy(_oports[fi]->_data);
  return 1;
}

int Module::setPortData (int p, Interface* intf)
{
  int fi = oportIdx(p);
  if(fi<0) return 0;
  _oports[fi]->_data.copy(*intf);
  return 1;
}

int Module::getPortProfile (int p, Types::Profile_out& prof)
{
  int fi = oportIdx(p);  
  if(fi<0) return 0; 
  prof = new Types::Profile(*(_oports[fi]->_profile));
  return 1;
}

int Module::setPortProfile (int p, const Types::Profile* prof)
{
  int fi = oportIdx(p);
  if(fi<0) return 0;

  if(_oports[fi]->_profile) delete _oports[fi]->_profile;

  _oports[fi]->_profile = new Types::Profile(*prof);
 
  return 1;
}

////////////////////////////////////////////////////////////////////////////////

Connection::Connection (int id)
  : _iport (NULL),
    _oport (NULL),
    _id    (id)
{
}

Connection::Connection (const Connection& c)
{
  copy(c);
}

Connection::~Connection ()
{
}

void Connection::copy (const Connection& c)
{
  if(this==&c) return;

  _iport = c._iport;
  _oport = c._oport;
  _id    = c._id;  
}

IPort* Connection::get_iport ()
{
  return _iport;
}

OPort* Connection::get_oport ()
{
  return _oport;
}

void Connection::connect_iport (IPort* p)
{
  _iport = p;
}

void Connection::connect_oport (OPort* p)
{
  _oport = p;
}

int Connection::get_id ()
{
  return _id;
}

////////////////////////////////////////////////////////////////////////////////

Port::Port (int id, Module* m)
  : _module (m),
    _id     (id)
{
}

Port::Port (const Port& p)
{
  copy(p);
}


Port::~Port ()
{
}

void Port::copy (const Port& p)
{
  if(this==&p) return;

  _connections = p._connections;
  _module      = p._module;
  _id          = p._id;
}

int Port::nconnections ()
{
  return _connections.size();
}

Connection* Port::connection(int idx)
{
  if(idx>=(int)_connections.size() || idx<0) return NULL;
  return _connections[idx];
}

void Port::add_connection (Connection* c)
{
  _connections.push_back(c);
}

Module* Port::get_module ()
{
  return _module;
}

int Port::get_id ()
{
  return _id;
}

////////////////////////////////////////////////////////////////////////////////

OPort::OPort (int id, Module* m)
  : Port (id, m)
{
  _profile = NULL;
}

OPort::OPort (const OPort& p)
  : Port(p)
{
  copy(p);
}

OPort::~OPort ()
{
}

void OPort::copy (const OPort& p)
{
  if(this==&p) return;
  
  _data    = p._data;
  if(_profile) delete _profile;
  _profile = new Types::Profile(*(p._profile));
}

int OPort::have_data ()
{
  return((_data.getInts()).size()      !=0 ||
	 (_data.getDoubles()).size()   !=0 ||
	 (_data.getStrings()).size()   !=0 ||
	 (_data.getInts1D()).size()    !=0 ||
	 (_data.getDoubles1D()).size() !=0 ||
	 (_data.getStrings1D()).size() !=0 );
}

int OPort::have_profile ()
{
  return(_profile != NULL);
}

////////////////////////////////////////////////////////////////////////////////

IPort::IPort (int id, Module* m)
  : Port (id, m)
{
}

IPort::IPort (const IPort& p)
  : Port(p)
{
  copy(p);
}

IPort::~IPort ()
{
}

void IPort::copy (const IPort& p)
{
  if(this==&p) return;
}
