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
 * File:          $RCSfile: VE_i.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_i.h"
#include <iostream>
  
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
         std::cout<<notification<<" :Raise called"<<std::endl;
         std::cout<<UIName_<<" :Raise called"<<std::endl;
         (*calcFlag) = true;  
      }
   }
  
