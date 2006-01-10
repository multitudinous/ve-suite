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
 * File:          $RCSfile: VE_i.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/VE_i.h"
#include <iostream>
#include <vpr/Sync/Guard.h>

using namespace VE_Xplorer;

// Implementation skeleton constructor
Body_UI_i::Body_UI_i (Body::Executive_ptr exec, std::string name)
  : executive_(Body::Executive::_duplicate(exec))
  {
    UIName_=name;
  }
  
// Implementation skeleton destructor
Body_UI_i::~Body_UI_i (void)
  {
  }
  
// This returns the latest network string 
// This is here so that the drawing sequence in vrjuggler 
// doesn't thread lock with executive CORBA calls
std::string Body_UI_i::GetNetworkString( void )
{
   vpr::Guard<vpr::Mutex> val_guard(stringBufferLock);
   std::string temp;
   temp.clear();
   if ( !networkStringBuffer.empty() )
   {
      std::vector< std::string >::iterator iter;
      iter = networkStringBuffer.begin();
      temp = (*iter);
      networkStringBuffer.erase( iter );
   }
   return temp;
}

// Complimentary function to the above function
void Body_UI_i::SetNetworkString( char* temp )
{
   vpr::Guard<vpr::Mutex> val_guard(stringBufferLock);
   networkStringBuffer.push_back( std::string( temp ) );
}

bool Body_UI_i::GetNetworkFlag( void )
{
   vpr::Guard<vpr::Mutex> val_guard( stringBufferLock );
   if ( networkStringBuffer.empty() )
   {
      return false;
   }
   else
   {
      return true;
   }
}

std::string Body_UI_i::GetStatusString( void )
{
   vpr::Guard<vpr::Mutex> val_guard( statusBufferLock );
   std::string temp;
   temp.clear();
   if ( !statusStringBuffer.empty() )
   {
      std::vector< std::string >::iterator iter;
      iter = statusStringBuffer.begin();
      temp = (*iter);
      statusStringBuffer.erase( iter );
   }
   return temp;
}

void Body_UI_i::UpdateNetwork (
    const char * network
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (network!=NULL)
      std::cout<<network<<std::endl;
    std::cout<<UIName_<<" :UpdateNetwork called"<<std::endl;
  }
  
void Body_UI_i::UpdateModuleUI (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (msg!=NULL)
      std::cout<<module_id<<" : "<<msg<<std::endl;
    std::cout<<UIName_<<" :UpdateModuleUI called"<<std::endl;
  }
  
void Body_UI_i::UpdateModuleResult (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (msg!=NULL)
      std::cout<<module_id<<" : "<<msg<<std::endl;
    std::cout<<UIName_<<" :UpdateModuleResult called"<<std::endl;
  }
  
void Body_UI_i::UpdateLinkContent (
    CORBA::Long id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    // Add your implementation here
    if (msg!=NULL)
      std::cout<<id<<" : "<<msg<<std::endl;
    std::cout<<UIName_<<" :UpdateLinkContent called"<<std::endl;
  }
  
void Body_UI_i::Raise (
    const char * notification
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
      // Add your implementation here
      if (notification!=NULL)
      {
         std::cout << "|\tNotification Message : " << notification << " : Raise called " << std::endl
                     << "|\tModule Being Called : " << UIName_ << " : Raise called"<<std::endl;
         
         vpr::Guard<vpr::Mutex> val_guard( statusBufferLock );
         {
            statusStringBuffer.push_back( std::string( notification ) );
         }
         
         std::string temp( notification );
         if ( !temp.compare(0,26,"Network execution complete") ||
              !temp.compare(0,30,"Successfully Scheduled Network" ) ||
              !temp.compare(0,22,"Connected to Executive") ||
              !temp.compare(0,17,"Error in Schedule") )
         {
            std::cout << "|\tGoing To Do Something" << std::endl;
            try 
            { 
               char* network = 0;
               network = executive_->GetNetwork();
               this->SetNetworkString( network );
            } 
            catch (CORBA::Exception &) 
            {
               std::cerr << "CFD DEBUG: cfdExecutive : no exec found! " << std::endl;
            }
         }
         else
         {
            std::cout << "|\tNot Going To Do Anything" << std::endl;
         }
         std::cout << "|\tEnd Raise " << std::endl;  
      }
   }
  
