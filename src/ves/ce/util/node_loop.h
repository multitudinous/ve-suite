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
#ifndef CE_UTILITIES_NODE_LOOP_H
#define CE_UTILITIES_NODE_LOOP_H
#include <ves/VEConfig.h>
#include <ves/ce/util/node_base.h>
#include <set>
#include <vector>

namespace VE_CE
{
namespace Utilities
{
class node_module;

class VE_CE_UTILS_EXPORTS node_loop : public node_base 
{
public:

	node_loop  (VE_CE::Utilities::Network *);
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
}
}
#endif
