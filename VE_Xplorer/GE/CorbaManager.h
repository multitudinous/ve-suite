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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_CORBA_MANAGER_H_
#define CFD_CORBA_MANAGER_H_

class cfdVjObsWrapper;
class cfdCommandArray;
class cfdSteadyStateVizHandler;
class cfdEnvironmentHandler;
class cfdModelHandler;
class cfdThread;
//class cfdCosNaming;

class CorbaManager
{
   public:
	   CorbaManager();		//Constructor
	   ~CorbaManager();		//destructor
	   void init( void * );		//Gets CORBA ready
	   void start();		//Creates a thread and runs server
	   //void bind();
	   //void regInterface(CORBA::Object_ptr obj,std::string objectId, std::string objectKind);	//Bind interface object
	   //CORBA::Object_var getInterface(std::string objectId, std::string objectKind);		 
	   //CORBA::Object_var obj;
      //void activate(PortableServer::ServantBase* myobj);
	   //void pman_activate();
      //VjObs_i* GetVjObs( void );
      cfdCommandArray* GetCommandArray( void );
      void GetCfdStateVariables( void );
      void PreFrameUpdate( void );
      //void GetUpdateClusterStateVariables( void );
      double GetShortArray( int );
	
      //cfdCosNaming* naming_context;
      void SetHandlers( cfdSteadyStateVizHandler*, 
                     cfdEnvironmentHandler*, cfdModelHandler* );
     cfdVjObsWrapper* _vjObs;
   private:
	   //CORBA::ORB_var orb;
      //PortableServer::POA_var poa;
       cfdThread* _thread;
	   /*
	   CORBA::Object_var obj;
      PortableServer::ServantBase* app;*/
};

#endif
