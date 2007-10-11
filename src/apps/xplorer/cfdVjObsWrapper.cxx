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
#include "cfdVjObsWrapper.h"
#include "VjObs_i.h"     //added for corba stuff

#include <orbsvcs/CosNamingC.h>
#include <tao/ORB.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>


#include <ves/xplorer/Xplorer_i.h>
#include <ves/xplorer/cfdCommandArray.h>
#include <ves/xplorer/cfdEnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/CommandHandler.h>

#include <ves/xplorer/cfdDebug.h>

#include <ves/open/xml/Command.h>

#include <iostream>
#include <boost/concept_check.hpp>

#include <vpr/IO/Socket/InetAddr.h>

using namespace CosNaming;
using namespace VE_Xplorer;
////////////////////////////////////////////////////////////////////////////////
cfdVjObsWrapper::cfdVjObsWrapper( void )
{
   _vjObs = new VjObs_i();
   m_xplorer = new Body_VEXplorer_i();
   isMaster = false;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVjObsWrapper::InitCluster( void )
{
   _vjObs->InitCluster();
}
////////////////////////////////////////////////////////////////////////////////
void cfdVjObsWrapper::GetUpdateClusterStateVariables( void )
{
   _vjObs->GetUpdateClusterStateVariables();
}
////////////////////////////////////////////////////////////////////////////////
cfdVjObsWrapper::~cfdVjObsWrapper( void )
{
   CosNaming::Name name(1);

   name.length(1);
   name[0].id   = CORBA::string_dup("Master");
   name[0].kind = CORBA::string_dup("VE_Xplorer");
   
   try
   {
      //vprDEBUG(vesDBG,0) 
         //<< "naming_context->unbind for CORBA Object  " 
         //<< std::endl << vprDEBUG_FLUSH;
      if ( !CORBA::is_nil( naming_context ) )
         naming_context->unbind( name );
   }
   catch( CosNaming::NamingContext::InvalidName& )
   {
      std::cerr << "Invalid name for CORBA Object  " << std::endl;
   }
   catch(CosNaming::NamingContext::NotFound& ex)
   {
      std::cerr << "Name not found for CORBA Object  " << ex.why << std::endl;
   }
   catch( ... )
   {
	   std::cerr << "Unknown exception." << std::endl;
   }

   //if ( !CORBA::is_nil( _orbPtr ) )
   //{
   //   std::cout << this->_orbPtr->id() << std::endl;
   //   this->_orbPtr->shutdown();
   //}

   vprDEBUG(vesDBG,1) 
      << " End VjObsWraper Destructor  " 
      << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void cfdVjObsWrapper::init( CosNaming::NamingContext* input, 
    CORBA::ORB* orbPtr, PortableServer::POA* child_poa, 
    PortableServer::POA* poa,int argc, char* argv[]  )
{
    //boost::ignore_unused_variable_warning( argc );
    //boost::ignore_unused_variable_warning( argv );
    bool isCluster = false;
    std::string masterhost;
   for(int i=1;i<argc;++i)
   {
      if ( std::string( argv[i] ) == std::string("-VESCluster") )
      {
         isCluster = true;
         masterhost = std::string( argv[i+1] );
         std::cout << "The cluster master is " << masterhost << std::endl;
         break;
      }
   }
   this->child_poa = child_poa;
   this->poa = poa;
   naming_context = input;

    if( isCluster )
    {
        std::cout << "----------------CLUSTER INFO-------------------" 
            << std::endl
            << "NOTE : Be sure to specify this GUID = " << std::endl
            << "       15c09c99-ed6d-4994-bbac-83587d4400d1 " << std::endl
            << "       in the application data config file." << std::endl;

        _vjObs->SetClusterMode( true );
        std::string name;
        vpr::InetAddr masterAddress;
        std::vector< vpr::InetAddr > tempAddrVec;
#if __VJ_version > 2000003
        tempAddrVec = vpr::InetAddr::getAllLocalAddrs( false );
#elif __VJ_version == 2000003
        masterAddress.getAllLocalAddrs( tempAddrVec, false );
#endif

        std::vector<std::string> toks;
        std::string tempHostname;
        for( size_t i = 0; i < tempAddrVec.size(); ++i )
        {
#if __VJ_version > 2000003
            tempHostname = tempAddrVec.at( i ).getHostname();
#elif __VJ_version == 2000003
             tempAddrVec.at( i ).getHostname( tempHostname );
#endif
            std::cout << "Host name is " << tempHostname <<std::endl;
            getStringTokens( tempHostname.c_str(),".", toks);
            //now toks[0] will be the short host name
            //the one without the domain name
            if( (tempHostname == masterhost) || (toks[0]==masterhost) )
            {
                std::cout<<"This is the master!"<<std::endl;
                isMaster = true;
                break;
            }
        }
    }
    else
    {
        isMaster = true;
        _vjObs->SetClusterMode( false );
    }

   if( isMaster || !isCluster )
   {
      //This is the old way of communication
      VjObs_var vjobs = this->_vjObs->_this();
      
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

	   ///This is the new way of communication
	   Body::VEXplorer_var xplorerCom = this->m_xplorer->_this();
      
      CosNaming::Name xplorerName;
      xplorerName.length(1);
      
      xplorerName[0].id   = (const char*) "Test";
      xplorerName[0].kind = (const char*) "VE_Xplorer";
      //Bind the object
      try
      {
         naming_context->bind(xplorerName, xplorerCom.in());
      }
      catch(CosNaming::NamingContext::AlreadyBound&)
      {
         naming_context->rebind(xplorerName, xplorerCom.in());
      }
      VE_Xplorer::CommandHandler::instance()->SetXplorer(m_xplorer);
   }
}
////////////////////////////////////////////////////////////////////////////////
cfdCommandArray* cfdVjObsWrapper::GetCommandArray( void )
{
   return _vjObs->_cfdArray;
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command* cfdVjObsWrapper::GetXMLCommand( void )
{
   return _vjObs->bufferCommand;
}
////////////////////////////////////////////////////////////////////////////////
double cfdVjObsWrapper::GetShortArray( int i )
{
   return _vjObs->cfdShort_data_array[ i ];
}
////////////////////////////////////////////////////////////////////////////////
void cfdVjObsWrapper::GetCfdStateVariables( void )
{
   _vjObs->GetCfdStateVariables();
}
////////////////////////////////////////////////////////////////////////////////
void cfdVjObsWrapper::PreFrameUpdate( void )
{
   _vjObs->PreFrameUpdate();
}
////////////////////////////////////////////////////////////////////////////////
// Frame sync variables used by osg only at this point
float cfdVjObsWrapper::GetSetAppTime( float x )
{
   return _vjObs->GetSetAppTime( x );
}
////////////////////////////////////////////////////////////////////////////////
long cfdVjObsWrapper::GetSetFrameNumber( long x )
{
   return _vjObs->GetSetFrameNumber( x );
}
////////////////////////////////////////////////////////////////////////////////
int cfdVjObsWrapper::getStringTokens(const char* buffer, char* delim, 
    std::vector<std::string> &toks)
{
   char* token;
   int i=0;
   std::string tempBuffer( buffer );
   char* temp = const_cast< char* >( tempBuffer.c_str() );
   token = strtok(temp, delim);

   toks.clear();
   while( token )
   {
      i++;
      toks.push_back(std::string(token));
      token = strtok(NULL, delim);
   }

   return i;
}
////////////////////////////////////////////////////////////////////////////////
bool cfdVjObsWrapper::IsMaster( void )
{
   return isMaster;
}
