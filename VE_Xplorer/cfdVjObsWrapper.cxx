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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdVjObsWrapper.h"
#ifdef _TAO
#include <orbsvcs/CosNamingC.h>
#endif
#include "VjObs_i.h"     //added for corba stuff
#include "cfdCommandArray.h"
#include "cfdSteadyStateVizHandler.h"
#include "cfdEnvironmentHandler.h"
#include "cfdModelHandler.h"
#include <vpr/Util/Debug.h>
#include <iostream>
using namespace std;
using namespace CosNaming;

cfdVjObsWrapper::cfdVjObsWrapper( void )
{
   _vjObs = new VjObs_i();
}

void cfdVjObsWrapper::InitCluster( void )
{
   _vjObs->InitCluster();
}

void cfdVjObsWrapper::GetUpdateClusterStateVariables( void )
{
   _vjObs->GetUpdateClusterStateVariables();
}

cfdVjObsWrapper::~cfdVjObsWrapper( void )
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
   }
   catch( CosNaming::NamingContext::InvalidName& )
   {
      cerr << "Invalid name for CORBA Object  " << endl;
   }
   catch(CosNaming::NamingContext::NotFound& ex)
   {
      cerr << "Name not found for CORBA Object  " << ex.why << endl;
   }
}

#ifdef _TAO
void cfdVjObsWrapper::init( CosNaming::NamingContext* input  )
#else
void cfdVjObsWrapper::init( CosNaming::NamingContext_ptr input  )
#endif
{
   naming_context = input;
#ifdef _CLUSTER
   int argc;
   char** argv;
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
         //return -1;
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

      VjObs_var vjobs = this->_vjObs->_this();
      //CORBA::String_var sior(orb->object_to_string(vjobs.in()));
      //cout << "|  IOR of the server(cfdApp) side : " << endl << sior << endl;
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
   
   VjObs_var vjobs = this->_vjObs->_this();
   if ( CORBA::is_nil( vjobs.in() ) )
     cout << "is nil " << endl;

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
}

cfdCommandArray* cfdVjObsWrapper::GetCommandArray( void )
{
   return _vjObs->_cfdArray;
}

double cfdVjObsWrapper::GetShortArray( int i )
{
   return _vjObs->cfdShort_data_array[ i ];
}

void cfdVjObsWrapper::GetCfdStateVariables( void )
{
   _vjObs->GetCfdStateVariables();
}

void cfdVjObsWrapper::PreFrameUpdate( void )
{
   _vjObs->PreFrameUpdate();
}

void cfdVjObsWrapper::SetHandlers( cfdSteadyStateVizHandler* _steadystateHandler, 
                           cfdEnvironmentHandler* _environmentHandler, 
                           cfdModelHandler* _modelHandler )
{
   _vjObs->SetHandlers( _steadystateHandler, _environmentHandler, _modelHandler );
}

int cfdVjObsWrapper::getStringTokens(char* buffer, char* delim, std::vector<std::string> &toks)
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
