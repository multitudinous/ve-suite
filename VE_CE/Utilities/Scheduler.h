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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CE_UTILITIES_SCHEDULER_H
#define CE_UTILITIES_SCHEDULER_H
#include "VE_Installer/include/VEConfig.h"
#include "VE_CE/Utilities/node_loop.h"
#include <vector>
#include <stack>
#include <set>

///Scheduler that uses all the previous classes
namespace VE_CE
{
namespace Utilities
{
class Module;
class Network;

class VE_CE_UTILS_EXPORTS Scheduler
{
public:
   Scheduler();
   Scheduler(Network*);
   ~Scheduler();

   void clear();  
   //void reset();
   void set_net( Network* );

   void sweep    (Module*);
   int  schedule (Module*);
   int  execute  (Module*);

   unsigned int snodes_size () { return _schedule_nodes._nodes.size(); }

   void print_schedule ();

   Network* _net;
  
private:

  int  visit     (int k, std::set<int> connid_ignore, std::vector<std::vector<int> >& sccs);
  void visit     (std::vector<std::vector<int> > adj, size_t k,  std::vector<int>& order);
  int  breakdown (std::vector<int> S, std::set<int> connid_ignore, node_loop &node_loop);

  std::vector<int> visit_val;
  std::stack<int>  visit_stack;

  int visit_id;

  node_loop _schedule_nodes;
};
}
}
#endif
