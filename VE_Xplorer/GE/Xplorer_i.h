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
#include "VE_Open/skel/moduleS.h"

class Body_VEXplorer_i
  : public virtual POA_Body::VEXplorer
{
public:
  // Constructor 
  Body_VEXplorer_i (void);
  
  // Destructor 
  virtual ~Body_VEXplorer_i (void);
  
  virtual
  char * GetStatusMessage (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetParams (
      const char * param
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetID (
      ::CORBA::Long id
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  ::CORBA::Long GetID (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetName (
      const char * name
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  char * GetName (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetNetwork (
      const char * network
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetCommand (
      const char * command
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void RegisterUI (
      const char * UIName,
      ::Body::UI_ptr ui
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void UnRegisterUI (
      const char * UIName
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
};
