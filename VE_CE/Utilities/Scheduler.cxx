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
#include "VE_CE/Utilities/Scheduler.h"
#include "VE_CE/Utilities/Module.h"
#include "VE_CE/Utilities/Network.h"
#include "VE_CE/Utilities/OPort.h"
#include "VE_CE/Utilities/IPort.h"
#include "VE_CE/Utilities/Connection.h"
#include "VE_CE/Utilities/node_module.h"
#include <iostream>
#include <queue>

using namespace VE_CE::Utilities;
////////////////////////////////////////////////////////////////////////////////
Scheduler::Scheduler ( )
  : _net            (NULL),
    _schedule_nodes (NULL)
{
}
////////////////////////////////////////////////////////////////////////////////
Scheduler::Scheduler (Network *n)
  : _net            (n),
    _schedule_nodes (n)
{
}
////////////////////////////////////////////////////////////////////////////////
Scheduler::~Scheduler ()
{
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::clear ()
{
  visit_val.clear();
  while(!visit_stack.empty()) visit_stack.pop();
  _schedule_nodes.clear();
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::reset ()
{
  int nmodules=_net->nmodules();
  int i, j;	
		    
  for(i=0;i<nmodules;i++) {
    _net->module(i)->_need_execute = 1;
    _net->module(i)->_return_state = 0;
    //_net->module(i)->_inputs.clear();
    _net->module(i)->_outputs.clear();
    _net->module(i)->_messages.clear();

    for(j=0; j<_net->module(i)->numOPorts(); j++)
      _net->module(i)->getOPort(j)->_data.clear();
  }
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::set_net (Network *n)
{
  clear();

  _net = n;
  _schedule_nodes.set_net(n);
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::sweep( Module* exclude )
{
   int nmodules=_net->nmodules();
   std::queue<Module *> needexecute;		

   // build queue of module ptrs to execute
   int i;			    
   for(i=0;i<nmodules;i++) 
   {
      Module* module=_net->module(i);
      if(module->_need_execute)	
         needexecute.push(module);
   }
   
   if(needexecute.empty()) 
   {
      return;
   }

   std::set<int> mod_been;

   std::vector<Connection*> to_trigger;
   while(!needexecute.empty())
   {
      Module* module = needexecute.front();
      needexecute.pop();

      if(mod_been.find(_net->module(module))==mod_been.end()) 
      {
         mod_been.insert(_net->module(module));

         // Add oports
         int no=module->numOPorts();
         int i;
         for(i=0;i<no;i++) 
         {
            OPort* oport=module->getOPort(i);
            int nc=oport->nconnections();
            for(int c=0;c<nc;c++) 
            {
               Connection* conn=oport->connection(c);
               IPort* iport=conn->get_iport();
               Module* m=iport->get_module();
               if(m != exclude && !m->_need_execute) 
               {
                  m->_need_execute=1;
                  needexecute.push(m);
               }
            }
         }

         // Now, look upstream...
         int ni=module->numIPorts();
         for(i=0;i<ni;i++) 
         {
            IPort* iport=module->getIPort(i);
            if(iport->nconnections()) 
            {
               Connection* conn=iport->connection(0);
               OPort* oport=conn->get_oport();
               Module* m=oport->get_module();
               if(!m->_need_execute) 
               {
                  if(m != exclude) 
                  {
                     // If this oport already has the data, add it
                     // to the to_trigger list...
                     if(oport->have_data())
                     {
                        to_trigger.push_back(conn);
                     } 
                     else 
                     {
                        m->_need_execute=true;
                        needexecute.push(m);
                     }
                  }
               }
            }
         }// end upstream for
      }
   } // end while
}
////////////////////////////////////////////////////////////////////////////////
int Scheduler::schedule (Module* mod)
{
  sweep(mod);

  int k;
  int nmodules = _net->nmodules();
  
  std::vector<int>      S;
  std::set<int> c_ignore;
  
  _schedule_nodes.clear();
  
  int st = 0;
  for(k=1; k<=nmodules; k++) {
    Module *m = _net->module(k-1);
    if(m->_need_execute) st++;
  }
  if(st==0) {
    std::cerr << "st was zero\n";
    return 0;
  }

  for(k=1; k<=nmodules; k++) {
    S.push_back(k);
  }
  
  if(breakdown(S, c_ignore, _schedule_nodes)) {
    std::cerr << "error in top breakdown\n";
    return 0;
  }
  
  if(_schedule_nodes._nodes.size() == 0)
    _schedule_nodes.add_node(new node_module(_net, 1));

  return 1;
}
////////////////////////////////////////////////////////////////////////////////
int Scheduler::execute (Module* mod)
{
  int st;
  static bool running = true; 

  int m;
  if(mod) {
    m = _net->module(mod)+1;
    if(mod->_return_state == 1) {
      mod->_return_state = 0;
      mod->_need_execute = true;
      running = false;
      return 0;
    }
  }
  else
    m = -2;

  st = _schedule_nodes.execute_mods(m, running);

  if(st > 0) running = true;
  else       running = false;

  return st;
}
////////////////////////////////////////////////////////////////////////////////
int Scheduler::visit(int k, std::set<int> connid_ignore,
		     std::vector< std::vector<int> >& sccs)
{
  visit_val[k] = ++visit_id;
  int min = visit_id;
    
  visit_stack.push(k);
  
  Module *module = _net->module(k-1);
  
  int m;
  for(int i=0; i<module->numOPorts(); i++) {
    OPort *oport = module->getOPort(i);
    for(int c=0; c<oport->nconnections(); c++) {
      Connection *conn = oport->connection(c);
      if(connid_ignore.find(conn->get_id())==connid_ignore.end()) {
	IPort *iport = conn->get_iport();
	Module *nmodule = iport->get_module();
	int index = _net->module(nmodule)+1;
	m = (!visit_val[index]) ? visit(index, connid_ignore, sccs) : visit_val[index];
	if (m < min) min = m;
      }
    } 
  }
  
  std::vector<int> scc;
  if(min == visit_val[k]) {
    scc.clear();
    do {
      m = visit_stack.top();
      visit_stack.pop();
      scc.push_back(m);
      visit_val[m] = _net->nmodules() + 1;
    } while(m != k);
    sccs.push_back(scc);
  }

  return min;  
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::visit(std::vector<std::vector<int> > adj, int k,
		      std::vector<int>& order)
{
  visit_val[k] = ++visit_id;
   
  for(int i=0; i<(int)adj[k].size(); i++)
    if(adj[k][i]==1 && visit_val[i]==0)
      visit(adj, i, order);
  
  order.insert(order.begin(), k);
}
////////////////////////////////////////////////////////////////////////////////
int Scheduler::breakdown (std::vector<int> S,
			  std::set<int> connid_ignore,
			  node_loop &nodes)
{
  int i, j, k;
  int nmodules=_net->nmodules();
   
  std::vector< std::vector<int> > sccs;

  if((int)S.size()==1) return 0;

  visit_id = 0;
  visit_val.clear(); visit_val.resize(nmodules+1);

  for(i=1; i<=nmodules; i++)     visit_val[i] = nmodules+1;
  for(i=0; i<(int)S.size(); i++) visit_val[S[i]] = 0;

  for(k=1; k<=nmodules; k++)
    if(!visit_val[k]) visit(k, connid_ignore, sccs);

  if((int)sccs.size()==2 &&
     (((int)sccs[0].size()==1 && (int)sccs[1].size()!=1 && _net->module(sccs[0][0]-1)->_is_feedback) ||
      ((int)sccs[1].size()==1 && (int)sccs[0].size()!=1 && _net->module(sccs[1][0]-1)->_is_feedback))) 
    {
      std::cerr << "Scheduler says: Unable (as of yet) to breakdown a loop\n";
      return 0;
    }

  std::set<int> c_ignore = connid_ignore;
  int cnt;
  for(i=0; i<(int)sccs.size(); i++)
    if((int)sccs[i].size() > 1) {
      cnt = 0;
      std::vector<int> fb_modules;
      for(k=0; k<(int)sccs[i].size(); k++)
	if(_net->module(sccs[i][k]-1)->_is_feedback) {
	  cnt++;
	  fb_modules.push_back(sccs[i][k]);
	}

      if(cnt == 0) {
	std::cerr << "Scheduler says: there exists a loop with no feedback module\n";
	return 1;
      }

      j = 0;
      bool done = false;
      node_loop nl(_net);

      int l;

      while(j<(int)fb_modules.size() && !done) {

	IPort *iport = _net->module(fb_modules[j]-1)->getFBPort();//IPort(1);
	for(k=0; k<iport->nconnections(); k++) {

	  Connection* conn=iport->connection(k);
	  OPort* oport=conn->get_oport();
	  Module* m=oport->get_module();
	  
	  bool found = false;
	  for(l=0; l<(int)sccs[i].size(); l++)
	    if(sccs[i][l] == _net->module(m)+1) found = true;
	  if(!found) {			
	    std::cerr << "Scheduler says: There exists feedback module connected to module outside of feedback loop\n";
	    return 1;
	  }
	  c_ignore.insert(iport->connection(k)->get_id());
	}

	if(breakdown(sccs[i], c_ignore, nl))
	  return 1;

	if(nl.mod_count()!=0) done = true;
	else
	  for(k=0; k<iport->nconnections(); k++) 
	    c_ignore.erase(iport->connection(k)->get_id());
	
	j++;
      }

      if(nl.mod_count()==0) {
	std::cerr << "Scheduler says: Had a nl.mod_count of zero !?\n";
	nodes._nodes.clear();
	return 0;
      }

      nodes.add_node(new node_loop(nl));
    }
    else
      nodes.add_node(new node_module(_net, sccs[i][0]));

  std::vector<std::vector<int> > adj(nodes.mod_count());
  for(k=0; k<nodes.mod_count(); k++)
    adj[k].resize(nodes.mod_count());

  for(k=0; k<nodes.mod_count(); k++)
    for(j=0; j<nodes.mod_count(); j++) {
      if(j==k) adj[k][j] = 0;
      else {
	std::set<int> k_outs;
	nodes._nodes[k]->get_outs(k_outs, c_ignore);
	std::set<int> j_mods;
	nodes._nodes[j]->get_mods(j_mods);
	      
	std::vector<int> ins;
	set_intersection(k_outs.begin(), k_outs.end(),
			 j_mods.begin(), j_mods.end(),
			 inserter(ins, ins.begin()));
	
	if((int)ins.size()>0) adj[k][j] = 1;
	else                  adj[k][j] = 0;
      }
    }

  visit_id = 0;
  visit_val.clear(); visit_val.resize(nodes.mod_count());

  for(i=0; i<nodes.mod_count(); i++) visit_val[i] = 0;
  
  std::vector<int> order;
  for(k=0; k<nodes.mod_count(); k++)
    if(!visit_val[k])
      visit(adj, k, order);
  
  if((int)order.size() != (int)nodes._nodes.size())
    std::cerr << "ERROR IN SCHEDULE\n";

  node_loop nodes_copy(nodes);
  nodes._nodes.clear();
  for(i=0; i<(int)order.size(); i++)
    if(nodes_copy._nodes[order[i]]->_type==0) 
      nodes.add_node((node_module*)nodes_copy._nodes[order[i]]);
    else
      nodes.add_node((node_loop*)nodes_copy._nodes[order[i]]);
  
  return 0;
}
////////////////////////////////////////////////////////////////////////////////
void Scheduler::print_schedule ()
{
   std::cout << std::endl;
   std::cout << "===========" << std::endl;

   for( int k=1; k <= _net->nmodules() ; k++) 
   {
      Module *m = _net->module(k-1);
      std::cout << k << " = " << m->get_id() << std::endl;
   }
  
   std::cout << std::endl; 
   _schedule_nodes.print_mods(); 
   std::cout << std::endl; 
   std::cout << "===========" << std::endl; 
}

