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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <CorbaManager.h>
#include <fstream>
#include <cstdlib>
#include <iostream>

#include <vpr/Util/Debug.h>


#include <VjObs_i.h>     //added for corba stuff

using namespace std;
//-------------------------------------------------------------
//CorbaManager methods.
//-------------------------------------------------------------

//Constructor
CorbaManager::CorbaManager()
{
   _vjObs = new VjObs_i();
	start();
}

CorbaManager::~CorbaManager()
{
   CosNaming::Name name(1);

   name.length(1);
   name[0].id   = (const char*) "Master";
   name[0].kind = (const char*) "VE_Xplorer";
   
   try
   {
      vprDEBUG(vprDBG_ALL,0) 
         << "naming_context->unbind for CORBA Object  " 
         << endl << vprDEBUG_FLUSH;
      naming_context->unbind( name );
      //naming_context->destroy();
   }
   catch( CosNaming::NamingContext::InvalidName& )
   {
      cerr << "Invalid name for CORBA Object  " << endl;
   }
   catch(CosNaming::NamingContext::NotFound& ex)
   {
      cerr << "Name not found for CORBA Object  " << ex.why << endl;
   }
   
   poa->destroy (1, 1);
   
   vprDEBUG(vprDBG_ALL,0) 
     << " destroying orb" << std::endl << vprDEBUG_FLUSH;

   orb->destroy();
}

VjObs_i* CorbaManager::GetVjObs( void )
{
   return _vjObs;
}
//init() Get CORBA ready to go
void CorbaManager::init( void * )
{
#ifdef _CLUSTER
   char buffer[1025];
   int ntoks, i;
   std::vector<std::string> toks;
   std::string hostfile;
   FILE * fhost;
   bool found=false;
   std::string masterhost="abbott";

   if (argc>1)
   {
      strcpy(buffer, argv[1]);
      ntoks=getStringTokens(buffer, "/", toks);
      //Now construct the name for the host file;
      hostfile="/";
      for (i=0; i<ntoks-1; i++)
      hostfile=hostfile+toks[i]+"/";
      hostfile+="component/host.config";
      cout<<"Here is the string for the hostfile :"<<hostfile<<endl;
      //Now we open that file and get the host name
      fhost=fopen(hostfile.c_str(), "r");
      if (fhost==NULL)
      {
         cout<<"Something bad in the path"<<endl;
         return -1;
      }

      while(!feof(fhost)&&!found)
      {
         fgets(buffer, 1024, fhost);
         ntoks=getStringTokens(buffer, "<>/ ", toks);
         for (i=0; i<ntoks; i++)
            if (toks[i]=="hostname" && i!=ntoks-1)
            {
               masterhost=toks[i+1];
               found=true;
               break;
            }
      }
      fclose(fhost);
   }
#endif // _CLUSTER

   int temp = 0;
   char** xargv;
   xargv = new char*[ temp ];
   //xargv[ 0 ] = "-ORBInitRef";
   //xargv[ 1 ] = "NameService=corbaname::cruncher.vrac.iastate.edu:2809";

#ifdef _TAO
   //xargv[ 0 ] = "-ORBInitRef";
   //xargv[ 1 ] = "NameService=file:///tmp/ns.ior";
   orb=CORBA::ORB_init( temp, xargv,"" );
#else
   orb=CORBA::ORB_init( temp, xargv );
   if ( CORBA::is_nil( orb.in() ) )
      exit(0);
#endif // _TAO
   //Here is the part to contact the naming service and get the reference for the executive
   CORBA::Object_var naming_context_object =
     orb->resolve_initial_references ("NameService"); 
   CORBA::String_var sior1(orb->object_to_string(naming_context_object.in ()));
   cout << "|  IOR of the server side : " << endl << sior1 << endl;

   naming_context =
       CosNaming::NamingContext::_narrow (naming_context_object.in ());
   
   
    //Here is the code to set up the server
    CORBA::Object_var poa_object =
      orb->resolve_initial_references ("RootPOA"); // get the root poa

    poa = PortableServer::POA::_narrow(poa_object.in());
    PortableServer::POAManager_var poa_manager = poa->the_POAManager ();
    poa_manager->activate ();
//   CORBA::String_var sior2(orb->object_to_string( poa.in() ) );
//   cout << "|  IOR of the server side 2 : " << endl << sior2 << endl;
#ifdef _CLUSTER
   char raw_hostname[256];
   std::string hostname;
   
   gethostname(raw_hostname, 255); //get the host name 
   hostname=raw_hostname;
   cout<<"Host name is "<<hostname<<endl;   
   getStringTokens(raw_hostname,".", toks);
   //now the toks[0] will be the short host name, which is the one without the domain name

   
   if (hostname==masterhost||toks[0]==masterhost)
   {
      cout<<"This is the master!"<<endl;

      VjObs_var vjobs = application->_this();
      CORBA::String_var sior(orb->object_to_string(vjobs.in()));
      cout << "|  IOR of the server(cfdApp) side : " << endl << sior << endl;
      CosNaming::Name name;
      name.length(1);

      name[0].id   = (const char*) "Master";
      name[0].kind = (const char*) "VE_Xplorer";
      //Bind the object
      try
      {
         naming_context->bind(name, vjobs.in());
      }
      catch(CosNaming::NamingContext::AlreadyBound& ex)
      {
         naming_context->rebind(name, vjobs.in());
      }
   }
#else // _CLUSTER
   VjObs_var vjobs = _vjObs->_this();
   if ( CORBA::is_nil( vjobs.in() ) )
     cout << "is nil " << endl;
   CORBA::String_var sior(orb->object_to_string( vjobs.in() ) );
   cout << "|  IOR of the server(cfdApp) side : " << endl << sior << endl;
   CosNaming::Name name;
   name.length(1);
   
   name[0].id   = (const char*) "Master";
   name[0].kind = (const char*) "VE_Xplorer";
   //Bind the object
   try
   {
      naming_context->bind(name, vjobs.in());
   }
   catch(CosNaming::NamingContext::AlreadyBound&)
   {
      naming_context->rebind(name, vjobs.in());
   }
#endif // _CLUSTER
   // If this isn't here the app won't work with TAO 
   cout << " end corba thread " << endl;
   orb->run();
   //while( 1 )
   {
   }
   cout << " end corba thread " << endl;
}

/*
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
   catch(CORBA::ORB::InvalidName& )
   {
		std::cerr<<"Service required invalid.  CorbaManager::init " << std::endl;
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
      catch(CosNaming::NamingContext::AlreadyBound&)
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
   catch(CORBA::COMM_FAILURE&)
   {
		std::cerr<<"COMM_FAILURE: no NameService found.  CorbaManager::init"<<std::endl;
	}
   catch(omniORB::fatalException& ex)
   {
	   cout << ex.errmsg() << endl;
		throw;
	}catch(...)
   {
	}
	//start();
}
*/
//start() Create a thread to start the server in
void CorbaManager::start()
{
	corba_run=new vpr::ThreadMemberFunctor<CorbaManager>(this, &CorbaManager::init );
	new_thread=new vpr::Thread(corba_run);
}

/*
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
	}catch(CosNaming::NamingContext::AlreadyBound&){
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
*/
int CorbaManager::getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks)
{
   char* token;
   int i=0;
   token = strtok(buffer, delim);

   toks.clear();
   while( token )
   {
      i++;
      toks.push_back(std::string(token));
      token = strtok(NULL, delim);
   }

   return i;
}
