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
 * File:          $RCSfile: Network_Exec.h,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef NETWORK_EXEC_H
#define NETWORK_EXEC_H
////////////////////////////////////////////////////////////////////////////////

node_module::node_module(Network *n, int m)
  : node_base(n, 0), _module(m)
{

}

/////////////

node_module::node_module(const node_module &nm)
  : node_base(nm._net, 0), _module(nm._module)
{
}

/////////////

node_module::~node_module()
{

}

/////////////

void node_module::get_mods(std::set<int> &mods)
{
  mods.clear();
  mods.insert(_module);
}

/////////////

void node_module::get_ins(std::set<int> &ins, std::set<int> connid_ignore)
{
  ins.clear();

  Module *module = _net->module(_module-1);
  
  int ni = module->numIPorts();

  for(int i=0; i<ni; i++) {
    IPort *iport = module->getIPort(i);
    int nc = iport->nconnections();
    for(int c=0; c<nc; c++) {
      Connection *conn = iport->connection(c);
      if(connid_ignore.find(conn->get_id())==connid_ignore.end()) {
	OPort *oport = conn->get_oport();
	Module *nmodule = oport->get_module();
	ins.insert(_net->module(nmodule)+1);
      }
    }
  }
}

/////////////

void node_module::get_outs(std::set<int> &outs, std::set<int> connid_ignore)
{
  outs.clear();

  Module *module = _net->module(_module-1);
  
  int no = module->numOPorts();

  for(int i=0; i<no; i++) {
    OPort *oport = module->getOPort(i);
    int nc = oport->nconnections();
    for(int c=0; c<nc; c++) {
      Connection *conn = oport->connection(c);
      if(connid_ignore.find(conn->get_id())==connid_ignore.end()) {
	IPort *iport = conn->get_iport();
	Module *nmodule = iport->get_module();
	outs.insert(_net->module(nmodule)+1);
      }
    } 
  }
}

/////////////

void node_module::print_mods()
{
  cerr << " " << _module;
}

/////////////

int node_module::execute_mods(int mod, bool running)
{
  Module *module = _net->module(_module-1);
  if(module->_need_execute) {
    // EXECUTING THIS MODULE
    module->_need_execute = false;
    return _module;
  }

  return 0;
}

/////////////

void node_module::need_execute ()
{
  Module *module = _net->module(_module-1);
  module->_need_execute = true;
}

/////////////

void node_module::clear_out_to (std::set<int> mods)
{
   Module *module = _net->module(_module-1);
   for(int i=0; i<module->numOPorts(); i++) 
   {
      OPort *oport = module->getOPort(i);
      for(int c=0; c<oport->nconnections(); c++) 
      {
         Connection *conn = oport->connection(c);
         IPort *iport = conn->get_iport();
         Module *nmodule = iport->get_module();
         int index = _net->module(nmodule)+1;
         if(mods.find(index) != mods.end()) 
         {	
	         // test
	         //cerr << "clearing data at inlet port for " << nmodule->get_id() << "\n";
	         if(iport->clear_data())
            {;}//cerr << "cleared some data\n";
	         //iport->reset();
	         //while(iport->have_data()) {
	         //  cerr << "finishing port\n";
	         //  iport->finish();
	         //}
         }
      }
   }
}
