/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: CorbaManager.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_CORBA_MANAGER_H_
#define CFD_CORBA_MANAGER_H_

//using namespace CORBA;
//{ 
//class ORB_var; 
//}
//namespace PortableServer { class POA; }
//namespace CosNaming { class NamingContext; }

#ifdef _TAO
#include <orbsvcs/CosNamingC.h>
#else
#include <omniORB4/CORBA.h>
#endif

#include <vector>
#include <string>

#include <tao/PortableServer/PortableServer.h>
#include <vpr/Thread/Thread.h>
class VjObs_i;
class cfdCommandArray;
class cfdSteadyStateVizHandler;
class cfdEnvironmentHandler;
class cfdModelHandler;

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
      VjObs_i* GetVjObs( void );
      cfdCommandArray* GetCommandArray( void );
      void GetCfdStateVariables( void );
      void PreFrameUpdate( void );
      //void GetUpdateClusterStateVariables( void );
      double GetShortArray( int );

      int getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks); // YANG, a string parsing utility, it is a not thread safe call.
	
      CosNaming::NamingContext_var naming_context;
      void SetHandlers( cfdSteadyStateVizHandler*, 
                     cfdEnvironmentHandler*, cfdModelHandler* );
   private:
	   CORBA::ORB_var orb;
      PortableServer::POA_var poa;
      VjObs_i* _vjObs;
      vpr::ThreadMemberFunctor<CorbaManager>* corba_run;
      vpr::Thread* new_thread;
	   /*
	   CORBA::Object_var obj;
      PortableServer::ServantBase* app;*/
};

#endif
