/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CE_UTILITIES_SCHEDULER_H
#define CE_UTILITIES_SCHEDULER_H
#include <VE_Installer/include/VEConfig.h>
#include <ves/ce/util/node_loop.h>
#include <vector>
#include <stack>
#include <set>

namespace VE_CE
{
namespace Utilities
{
class Module;
class Network;

///Scheduler that uses all the previous classes
class VE_CE_UTILS_EXPORTS Scheduler
{
public:
    ///Construtor
    Scheduler();
    ///??
    Scheduler(Network*);
    ///Destructor
    ~Scheduler();

    ///clear the schedule
    void clear();  
    ///Reset the visit variables so that the schedule can be run again
    void reset();
    ///??
    void set_net( Network* );

    ///??
    void sweep    (Module*);
    ///??
    int  schedule (Module*);
    ///??
    int  execute  (Module*);

    ///??
    unsigned int snodes_size () { return _schedule_nodes._nodes.size(); }

    ///Printthe schedule to verify the correct network has been recreated
    void print_schedule ();
    ///Raw Network
    Network* _net;
  
private:
    int visit( int k, std::set<int> connid_ignore, 
        std::vector<std::vector<int> >& sccs);
    void visit( std::vector<std::vector<int> > adj, 
        size_t k,  std::vector<int>& order);
    int  breakdown( std::vector<int> S, std::set<int> connid_ignore, 
        node_loop &node_loop);

    std::vector<int> visit_val;
    std::stack<int>  visit_stack;

    int visit_id;

    node_loop _schedule_nodes;
};
}
}
#endif
