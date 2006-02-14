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
#ifndef CE_UTILITIES_MODULE_H
#define CE_UTILITIES_MODULE_H
class Module 
{
public:
   Module( int, Network* );
   Module( const Module& );
   ~Module ();

   void copy( const Module& );

   ///return number of output ports
   int numOPorts( void );
   ///return number of input ports
   int numIPorts( void );

   ///Get the vector index for the specific input port id 
   int iportIdx( int idx );
   ///Get the vector index for the specific output port id 
   int oportIdx( int idx );

   ///Add input port
   void addIPort (int, Connection*);
   ///Add output port
   void addOPort (int, Connection*);

   ///Get the ith output port
   OPort* getOPort( int i );
   ///Get the ith input port
   IPort* getIPort( int i );
  
   ///Get Feedback Port
   IPort* getFBPort ();

   ///Get output port data for the specific port
   int getPortData( int, Interface& );
   ///Set output port data for the specific port
   int setPortData( int, Interface* );

   int getPortProfile (int, Types::Profile_out&);
   int setPortProfile (int, const Types::Profile*);

   ///Get the ID for the module
   int get_id();

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
   //Input ports for the module
   std::vector<IPort*> _iports;
   //Output ports for the module
   std::vector<OPort*> _oports;

   ///ID for the particular module
   int _id;
};
#endif
