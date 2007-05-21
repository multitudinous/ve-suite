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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/GUIPlugin/Xplorer_i.h"
#include "VE_Conductor/GUIPlugin/OrbThread.h"

  
// Implementation skeleton constructor
Body_Xplorer_i::Body_Xplorer_i (Body::Executive_ptr exec, std::string name)
  : UIName_(name), executive_(Body::Executive::_duplicate(exec))
{
    UIName_=name;
}
  
// Implementation skeleton destructor
Body_Xplorer_i::~Body_Xplorer_i (void)
  {
  }
  
void Body_Xplorer_i::UpdateNetwork (
    const char * network
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    throw CORBA::NO_IMPLEMENT();
  }
  
void Body_Xplorer_i::UpdateModuleUI (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    throw CORBA::NO_IMPLEMENT();
  }
  
void Body_Xplorer_i::UpdateModuleResult (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    throw CORBA::NO_IMPLEMENT();
  }
  
void Body_Xplorer_i::UpdateLinkContent (
    CORBA::Long id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    throw CORBA::NO_IMPLEMENT();
  }
  
void Body_Xplorer_i::Raise (
    const char * notification
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ))
  {
    throw CORBA::NO_IMPLEMENT();
  }
  
void Body_Xplorer_i::SetXplorerData (
    const char * xplorerData
  )
  ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ))
{
	// Add your implementation here
    throw CORBA::NO_IMPLEMENT();
}

void Body_Xplorer_i::SetLogWindow( PEThread* logWindow )
{ 
	  this->logWindow = logWindow;
}
