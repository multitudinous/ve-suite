/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: Network_Exec.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef NETWORK_EXEC_H
#define NETWORK_EXEC_H

#include "VE_Conductor/Framework/interface.h"
#include <vector>
#include "moduleS.h"

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

  int have_data    ();
  int have_profile ();

  Interface      _data;
  Types::Profile *_profile;

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

  bool hasGeomInfo();

  int get_id ();

  std::string _name;

  int _need_execute;
  int _return_state;
  int _is_feedback;
  
  int _type;
  int _category;

  Interface _inputs;
  Interface _geominputs;
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

  int getGeomInput(int, Interface&);
  int setGeomInput(int, Interface*);

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
 
