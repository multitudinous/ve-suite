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
 * File:          $RCSfile: CorbaManager.cxx,v $
 * Date modified: $Date: 2004/03/23 16:38:01 $
 * Version:       $Revision: 1.3 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <CorbaManager.h>
#include <fstream>
#include <cstdlib>
#include <iostream>

using std::cout;
using std::endl;
//-------------------------------------------------------------
//CorbaManager methods.
//-------------------------------------------------------------

//Constructor
CorbaManager::CorbaManager()
{
	init();
}

//init() Get CORBA ready to go
void CorbaManager::init()
{
	int temp=0;
#ifdef __OMNIORB3__		
	orb=CORBA::ORB_init(temp,0,"omniORB3");
#endif

#ifdef __OMNIORB4__		
	orb=CORBA::ORB_init(temp,0,"omniORB4");
#endif

	obj = orb->resolve_initial_references("RootPOA");
	poa = PortableServer::POA::_narrow(obj);
}


void CorbaManager::bind()
{
	CosNaming::NamingContext_var rootContext;
	try
   {
		CORBA::Object_var initServ;
		initServ=orb->resolve_initial_references("NameService");
		rootContext=CosNaming::NamingContext::_narrow(initServ);
		
		//if(CORBA::is_nil(poa))
		if(CORBA::is_nil(rootContext))
      {
			std::cerr<<"Failed to narrow. CorbaManager::init"<<std::endl;
		}
	}
   catch(CORBA::ORB::InvalidName& ex)
   {
		std::cerr<<"Service required invalid.  CorbaManager::init"<<std::endl;
	}

   cout << "|   IOR of the omni nameserver : " << endl << 
            orb->object_to_string( rootContext ) << endl;
	try
   {
		CosNaming::Name contextName;
		contextName.length(1);
		contextName[0].id=CORBA::string_dup("test");
		contextName[0].kind=CORBA::string_dup("my_context");
		//contextName[0].id=(const char*) "test";
		//contextName[0].kind=(const char*) "my_context";
		try
      {
         //cout << contextName[0].id << " : " << contextName[0].kind << endl;
			testContext=rootContext->bind_new_context(contextName);
		}
      catch(CosNaming::NamingContext::AlreadyBound& ex)
      {
			CORBA::Object_var tmpobj;
			tmpobj=rootContext->resolve(contextName);
			testContext=CosNaming::NamingContext::_narrow(tmpobj);
			if(CORBA::is_nil(testContext))
         {
				std::cerr<<"Failed to narrow. CorbaManager::init"<<std::endl;
			}
		}
      //cout << " testcontext : " << orb->object_to_string( testContext ) << endl;
	}
   catch(CORBA::COMM_FAILURE& ex)
   {
		std::cerr<<"COMM_FAILURE: no NameService found.  CorbaManager::init"<<std::endl;
	}
   catch(omniORB::fatalException& ex)
   {
		throw;
	}catch(...)
   {
	}
	//start();
}

//start() Create a thread to start the server in
void CorbaManager::start()
{
/*
   int dummy=0;
	vjThreadMemberFunctor<CorbaManager>* corba_run=new vjThreadMemberFunctor<CorbaManager>(this, &CorbaManager::run, &dummy);
	vjThread* new_thread=new vjThread(corba_run);
*/
}


void CorbaManager::activate(PortableServer::ServantBase* myobj)
{
	PortableServer::ObjectId_var myid = poa->activate_object(myobj);
   app = myobj;
   //obj = myobj->_this();
}

void CorbaManager::pman_activate()
{
	//app->_remove_ref();

   PortableServer::POAManager_var pman = poa->the_POAManager();
	pman->activate();
   //orb->run();
   std::cout <<"|   Successfully connected to the omniNames Server"<< std::endl;
}

//regInterface() Bind interface to the running name service
void CorbaManager::
regInterface(CORBA::Object_ptr obj, std::string objectId, std::string objectKind)
{
	CosNaming::Name objectName;
	objectName.length(1);
	objectName[0].id=CORBA::string_dup(objectId.c_str());
	objectName[0].kind=CORBA::string_dup(objectKind.c_str());
	//objectName[0].id=(const char*) "Hello";
	//objectName[0].kind=(const char*) "Object";
   //cout << objectName[0].id << " : " << objectName[0].kind << endl;
   //cout << objectId.c_str() << " : " << objectKind.c_str() << endl;
	try{
		testContext->bind(objectName,obj);
	}catch(CosNaming::NamingContext::AlreadyBound& ex){
		testContext->rebind(objectName,obj);
	}
}

//getInterface() Get a reference to a reference not bound by the server
CORBA::Object_var CorbaManager::
getInterface(std::string objectId, std::string objectKind)
{
	int temp=0;

	CORBA::ORB_ptr orb=CORBA::ORB_init(temp,0,"omniORB4");
	CORBA::Object_var obj;
	CosNaming::NamingContext_var rootContext;
	CORBA::Object_var initServ;
	initServ=orb->resolve_initial_references("NameService");
	rootContext=CosNaming::NamingContext::_narrow(initServ);
	CosNaming::Name name;
	name.length(1);
	name[0].id=CORBA::string_dup(objectId.c_str());
	name[0].kind=CORBA::string_dup(objectKind.c_str());
	CORBA::Object_ptr objptr;
	objptr=rootContext->resolve(name);
	obj=objptr;	

	return obj;
}
