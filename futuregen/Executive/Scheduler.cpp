
#include "Scheduler.h"
#include "Network_Exec.h"
#include "string_ops.h"

#include <iostream>
#include <queue>

#include <algo.h>

using namespace std;

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

void Scheduler::set_net (Network *n)
{
  clear();

  _net = n;
  _schedule_nodes.set_net(n);
}

////////////////////////////////////////////////////////////////////////////////

void Scheduler::sweep (Module* exclude)
{
  int nmodules=_net->nmodules();
  queue<Module *> needexecute;		

  // build queue of module ptrs to execute
  int i;			    
  for(i=0;i<nmodules;i++) {
    Module* module=_net->module(i);
    if(module->_need_execute)	
      needexecute.push(module);
  }
  if(needexecute.empty()) {
    return;
  }

  std::set<int> mod_been;

  vector<Connection*> to_trigger;
  while(!needexecute.empty()){
    Module* module = needexecute.front();
    needexecute.pop();
    
    if(mod_been.find(_net->module(module))==mod_been.end()) {
      mod_been.insert(_net->module(module));

      // Add oports
      int no=module->numOPorts();
      int i;
      for(i=0;i<no;i++) {
	OPort* oport=module->getOPort(i);
	int nc=oport->nconnections();
	for(int c=0;c<nc;c++) {
	  Connection* conn=oport->connection(c);
	  IPort* iport=conn->get_iport();
	  Module* m=iport->get_module();
	  if(m != exclude && !m->_need_execute) {
	    m->_need_execute=1;
	    needexecute.push(m);
	  }
	}
      }
      
      // Now, look upstream...
      int ni=module->numIPorts();
      for(i=0;i<ni;i++) {
	IPort* iport=module->getIPort(i);
	if(iport->nconnections()) {
	  Connection* conn=iport->connection(0);
	  OPort* oport=conn->get_oport();
	  Module* m=oport->get_module();
	  if(!m->_need_execute) {
	    if(m != exclude) {
	      // If this oport already has the data, add it
	      // to the to_trigger list...
	      if(oport->have_data()){
		to_trigger.push_back(conn);
	      } else {
		m->_need_execute=true;
		needexecute.push(m);
	      }
	    }
	  }
	}
      }
      
    }
    
  }
  
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
    cerr << "st was zero\n";
    return 0;
  }

  for(k=1; k<=nmodules; k++) {
    S.push_back(k);
  }
  
  if(breakdown(S, c_ignore, _schedule_nodes)) {
    cerr << "error in top breakdown\n";
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
		     std::vector<vector<int> >& sccs)
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
   
  std::vector<vector<int> > sccs;

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
      cerr << "Scheduler says: Unable (as of yet) to breakdown a loop\n";
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
	cerr << "Scheduler says: there exists a loop with no feedback module\n";
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
	    cerr << "Scheduler says: There exists feedback module connected to module outside of feedback loop\n";
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
	cerr << "Scheduler says: Had a nl.mod_count of zero !?\n";
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
    cerr << "ERROR IN SCHEDULE\n";

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
  int k, nmodules = _net->nmodules();

  cerr << "\n===========\n";
  for(k=1; k<=nmodules; k++) {
    Module *m = _net->module(k-1);
    cerr << k << " = " << m->get_id() << endl;
  }
  
  cerr << "\n"; _schedule_nodes.print_mods(); cerr << "\n";
  cerr << "===========\n";
}

////////////////////////////////////////////////////////////////////////////////

node_base::node_base(Network *n, int t)
  : _net(n), _type(t)
{
}

/////////////

node_base::node_base(const node_base &nb)
{
  _net = nb._net;
}

/////////////

node_base::~node_base()
{

}
/////////////

void node_base::set_net (Network *n)
{
  _net = n;
}

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
  for(int i=0; i<module->numOPorts(); i++) {
    OPort *oport = module->getOPort(i);
    for(int c=0; c<oport->nconnections(); c++) {
      Connection *conn = oport->connection(c);
      IPort *iport = conn->get_iport();
      Module *nmodule = iport->get_module();
      int index = _net->module(nmodule)+1;
      if(mods.find(index) != mods.end()) {
	
	// test
	//cerr << "clearing data at inlet port for " << nmodule->get_id() << "\n";
	if(iport->clear_data())
	  ;//cerr << "cleared some data\n";
	//iport->reset();
	//while(iport->have_data()) {
	//  cerr << "finishing port\n";
	//  iport->finish();
	//}
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////

node_loop::node_loop(Network *n)
  : node_base(n, 1)
{
}

/////////////

node_loop::node_loop(const node_loop &nl)
  : node_base(nl._net, 1)
{
  clear();
		
  for(int i=0; i<(int)nl._nodes.size(); i++)
    if(nl._nodes[i]->_type==0) add_node((node_module*)nl._nodes[i]);
    else                       add_node((node_loop*)nl._nodes[i]);
}

/////////////

node_loop::~node_loop()
{
  clear();
}

/////////////

void node_loop::clear()
{
  for(int i=0; i<(int)_nodes.size(); i++)
    delete _nodes[i];
  _nodes.clear();
}

/////////////

void node_loop::add_node(const node_module *nm)
{
  _nodes.push_back(new node_module(*nm));
}

/////////////

void node_loop::add_node(const node_loop *nl)
{
  _nodes.push_back(new node_loop(*nl));

}

/////////////

void node_loop::get_mods(std::set<int> &mods)
{
  mods.clear();
  
  std::set<int> temp_mods;
  std::set<int> new_mods;

  for(int i=0; i<(int)_nodes.size(); i++) {
    _nodes[i]->get_mods(temp_mods);
    set_union(mods.begin(), mods.end(),
	      temp_mods.begin(), temp_mods.end(),
	      inserter(new_mods, new_mods.begin()));
    mods = new_mods;
  }
}

/////////////

void node_loop::get_ins(std::set<int> &ins, std::set<int> connid_ignore)
{
  ins.clear();
  
  std::set<int> mods;
  get_mods(mods);

  std::set<int> temp_ins;
  std::set<int> new_ins;

  for(int i=0; i<(int)_nodes.size(); i++) {
    _nodes[i]->get_ins(temp_ins, connid_ignore);
    set_union(ins.begin(), ins.end(),
	      temp_ins.begin(), temp_ins.end(),
	      inserter(new_ins, new_ins.begin()));
    ins = new_ins;
  }

  new_ins.clear();

  set_difference(ins.begin(), ins.end(),
		 mods.begin(), mods.end(),
		 inserter(new_ins, new_ins.begin()));
  ins = new_ins;
}

/////////////

void node_loop::get_outs(std::set<int> &outs, std::set<int> connid_ignore)
{
  outs.clear();
  
  std::set<int> mods;
  get_mods(mods);

  std::set<int> temp_outs;
  std::set<int> new_outs;

  for(int i=0; i<(int)_nodes.size(); i++) {
    _nodes[i]->get_outs(temp_outs, connid_ignore);
    set_union(outs.begin(), outs.end(),
	      temp_outs.begin(), temp_outs.end(),
	      inserter(new_outs, new_outs.begin()));
    outs = new_outs;
  }
  
  new_outs.clear();

  set_difference(outs.begin(), outs.end(),
		 mods.begin(), mods.end(),
		 inserter(new_outs, new_outs.begin()));
  outs = new_outs;
}

/////////////

void node_loop::print_mods()
{
  cerr << " (";
  for(int i=0; i<(int)_nodes.size(); i++)
    _nodes[i]->print_mods();
  cerr << " )";
}

/////////////

int node_loop::execute_mods(int mod, bool running)
{
  int r;

  for(int i=0; i<(int)_nodes.size()-1; i++) {
    r = _nodes[i]->execute_mods(mod, running);
    if(r > 0) return r;
  }
 
  r = _nodes[(int)_nodes.size()-1]->execute_mods(mod, running);

  bool is_fb = false;
  if(_nodes[0]->_type==0)
    if(_net->module(((node_module*)_nodes[0])->_module-1)->_is_feedback)
      is_fb = true;
  
  if(r > 0 && is_fb) {
    if(_net->module(((node_module*)_nodes[0])->_module-1)->_return_state == 3) {
      _net->module(((node_module*)_nodes[0])->_module-1)->_return_state = 0;
      // erase _node[0]'s feedback input
      //mike, _net->module(((node_module*)_nodes[0])->_module-1)->getIPort(1)->reset();
      //mike, while(_net->module(((node_module*)_nodes[0])->_module-1)->getIPort(1)->have_data())
      //mike, _net->module(((node_module*)_nodes[0])->_module-1)->getIPort(1)->finish();
    } else {
      // erase all outputs to any nodes outside of this loop
      std::set<int> mods;
      get_mods(mods);
      
      std::set<int> c_ignore;
      
      std::set<int> outs;
      get_outs(outs, c_ignore);
      
      std::set<int> dif;
      set_difference(outs.begin(), outs.end(),
		     mods.begin(), mods.end(),
		     inserter(dif, dif.begin()));

      // test
      //std::set<int>::iterator iter;
      //cerr << "REMOVING INPUTS TO: \n";
      //for(iter=dif.begin(); iter!=dif.end(); iter++)
      //	cerr << _net->module((*iter)-1)->get_id() << "\n";

      clear_out_to(dif);      

      // reschedule loop nodes
      need_execute();
    }
  }

  return r;
}

/////////////

void node_loop::need_execute ()
{
  for(int i=0; i<(int)_nodes.size(); i++)
    _nodes[i]->need_execute();
}
   
/////////////

void node_loop::clear_out_to (std::set<int> mods)
{
  for(int i=0; i<(int)_nodes.size(); i++)
    _nodes[i]->clear_out_to(mods);
}

