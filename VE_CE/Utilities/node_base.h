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

class node_base 
{
public:

   node_base( Network *, int );
   node_base( const node_base& );
   virtual ~node_base();

   void set_net( Network* );

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
