
#ifndef NETWORK_EXEC_H
#define NETWORK_EXEC_H

#include "interface.h"
#include <vector>
#include "moduleS.h"

using namespace std;

class Module;
class Connection;
class IPort;
class OPort;
class Network;

////////////////////////////////////////////////////////////////////////////////

class Port {

public:

  Port  (int, Module*);
  Port  (const Port&);
  ~Port ();

  void copy (const Port&);

  int         nconnections   ();
  Connection* connection     (int);
  void        add_connection (Connection*);

  Module* get_module ();
  int     get_id     ();

protected:

  std::vector<Connection*> _connections;

  Module* _module;
  int     _id;
};

////////////////////////////////////////////////////////////////////////////////

class IPort : public Port {

public:
  
  IPort  (int, Module*);
  IPort  (const IPort&);
  ~IPort ();

  void copy (const IPort&);

  int clear_data () { return 1; };
};

////////////////////////////////////////////////////////////////////////////////

class OPort : public Port {

public:

  OPort  (int, Module*);
  OPort  (const OPort&);
  ~OPort ();
  
  void copy (const OPort&);

  int have_data ();

  Interface      _data;
  Types::Profile _profile;

protected:
  
};

////////////////////////////////////////////////////////////////////////////////

class Connection {
  
public:

  Connection  (int);
  Connection  (const Connection&);
  ~Connection ();
  
  void copy (const Connection&);

  IPort* get_iport ();
  OPort* get_oport ();

  void connect_iport (IPort*);
  void connect_oport (OPort*);

  int get_id ();

private:

  IPort* _iport;
  OPort* _oport;

  int _id;
};

////////////////////////////////////////////////////////////////////////////////

class Module {

public:
  
  Module  (int, Network*);
  Module  (const Module&);
  ~Module ();

  void copy (const Module&);

  int numOPorts ();
  int numIPorts ();

  int iportIdx (int);
  int oportIdx (int);

  void addIPort (int, Connection*);
  void addOPort (int, Connection*);

  OPort* getOPort (int);
  IPort* getIPort (int);
  
  IPort* getFBPort ();

  int getPortData (int, Interface&);
  int setPortData (int, Interface*);

  int getPortProfile (int, Types::Profile_out&);
  int setPortProfile (int, const Types::Profile*);

  int get_id ();

  std::string _name;

  int _need_execute;
  int _return_state;
  int _is_feedback;
  
  int _type;
  int _category;

  Interface _inputs;
  Interface _outputs;
  Interface _messages;

  Network* _net;

private:

  std::vector<IPort*> _iports;
  std::vector<OPort*> _oports;

  int _id;
};

////////////////////////////////////////////////////////////////////////////////

class Network {

public:
  
  Network  ();
  ~Network ();

  void clear ();
  int  parse (Interface *);

  int nmodules   ();
  void         add_module (int, std::string);
  int          module     (Module*);
  Module*      module     (int);
  int moduleIdx  (int);

  int addIPort (int, int, Connection*);
  int addOPort (int, int, Connection*);

  int getInput (int, Interface&);
  int setInput (int, Interface*);

  int getOutput (int, Interface&);
  int setOutput (int, Interface*);

  int getMessage (int, Interface&);
  int setMessage (int, Interface*);

  int getPortData (int, int, Interface&);
  int setPortData (int, int, Interface*);

  int getPortProfile (int, int, Types::Profile_out&);
  int setPortProfile (int, int, const Types::Profile*);

  std::vector<Connection*> _connections;

protected:

  std::vector<Module*> _module_ptrs;

};

#endif
 
