
#ifndef SCHEDULER_H
#define SCHEDULER_H

#include <string>
#include <vector>
#include <stack>
#include <set>

using namespace std;

class Network;
class Module;
class OPort;

////////////////////////////////////////////////////////////////////////////////

class node_base {

 public:

  node_base (Network *, int);
  node_base (const node_base&);
  virtual ~node_base ();
  
  void set_net (Network*);

  virtual int  mod_count ()=0;
  virtual void get_mods (std::set<int> &)=0;
  virtual void get_ins (std::set<int> &, std::set<int> connid_ignore)=0;
  virtual void get_outs (std::set<int> &, std::set<int> connid_ignore)=0;
  virtual void print_mods ()=0;
  virtual int  execute_mods (int, bool)=0;
  virtual void need_execute ()=0;
  virtual void clear_out_to (std::set<int>)=0;
  
  Network *_net;
  int     _type;

};

////////////////////////////////////////////////////////////////////////////////

class node_module : public node_base {

 public:

  node_module  (Network *, int);
  node_module  (const node_module&);
  ~node_module ();
  
  virtual int  mod_count () { return 1; };
  virtual void get_mods (std::set<int> &);
  virtual void get_ins (std::set<int> &, std::set<int> connid_ignore);
  virtual void get_outs (std::set<int> &, std::set<int> connid_ignore);
  virtual void print_mods ();
  virtual int  execute_mods (int, bool);
  virtual void need_execute ();
  virtual void clear_out_to (std::set<int>);
  
  int _module;

};

////////////////////////////////////////////////////////////////////////////////

class node_loop : public node_base {

 public:

  node_loop  (Network *);
  node_loop  (const node_loop&);
  ~node_loop ();
  
  virtual int  mod_count () { return (int)_nodes.size(); };
  virtual void get_mods (std::set<int> &);
  virtual void get_ins (std::set<int> &, std::set<int> connid_ignore);
  virtual void get_outs (std::set<int> &, std::set<int> connid_ignore);
  virtual void print_mods ();
  virtual int  execute_mods (int, bool);
  virtual void need_execute ();
  virtual void clear_out_to (std::set<int>);
  
  void clear ();

  void add_node (const node_module*);
  void add_node (const node_loop*);
  
  std::vector<node_base*> _nodes;

};

////////////////////////////////////////////////////////////////////////////////

class Scheduler {

public:

  Scheduler  ();
  Scheduler  (Network*);
  ~Scheduler ();
  
  void clear   ();  
  void set_net (Network *);

  void sweep    (Module*);
  int  schedule (Module*);
  int  execute  (Module*);

  void print_schedule ();

  Network* _net;
  
private:

  int  visit     (int k, std::set<int> connid_ignore, std::vector<vector<int> >& sccs);
  void visit     (std::vector<std::vector<int> > adj, int k,  std::vector<int>& order);
  int  breakdown (std::vector<int> S, std::set<int> connid_ignore, node_loop &node_loop);

  std::vector<int> visit_val;
  std::stack<int>  visit_stack;

  int visit_id;

  node_loop _schedule_nodes;
};

#endif

