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
 * Date modified: $Date: 2007-05-21 22:29:24 -0500 (Mon, 21 May 2007) $
 * Version:       $Rev: 7749 $
 * Author:        $Author: mccdo $
 * Id:            $Id: cfdApp.h 7749 2007-05-22 03:29:24Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef BODY_AMI_VEXPLORER_HANDLER_I_H
#define BODY_AMI_VEXPLORER_HANDLER_I_H
/*!\file Body_AMI_VEXplorerHandler_i.h
*Interface for sending information from Xplorer Asynchronously  
*/

/*!\class Body_AMI_VEXplorerHandler_i
*
*/

#include "VE_Open/skel/moduleS.h"

class VE_OPEN_MODULE_EXPORTS Body_AMI_VEXplorerHandler_i
  : public virtual POA_Body::AMI_VEXplorerHandler
{
public:
    ///Constructor 
    Body_AMI_VEXplorerHandler_i(void);
  
    ///Destructor 
    virtual ~Body_AMI_VEXplorerHandler_i(void);
  
    ///Get the status message
    virtual
    void GetStatusMessage( const char * ami_return_val )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Get the status message
    virtual
    void GetStatusMessage_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Set the params 
    virtual
    void SetParams( )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Set the params 
    virtual
    void SetParams_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Set the ID
    virtual
    void SetID( )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Set the ID
    virtual
    void SetID_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Get the ID
    virtual
    void GetID( ::CORBA::Long ami_return_val )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Get the ID
    virtual
    void GetID_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Set the name
    virtual
    void SetName( )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Set the name
    virtual
    void SetName_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Get the name
    virtual
    void GetName ( const char * ami_return_val)
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Get the name
    virtual
    void GetName_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Set the network
    virtual
    void SetNetwork( )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Set the network
    virtual
    void SetNetwork_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Set the command
    virtual
    void SetCommand( )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Set the command
    virtual
    void SetCommand_excep(
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///Register the UI
    virtual
    void RegisterUI( )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///Register the UI
    virtual
    void RegisterUI_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
  
    ///UnRegister the UI
    virtual
    void UnRegisterUI( )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
    ///UnRegister the UI
    virtual
    void UnRegisterUI_excep (
      ::Messaging::ExceptionHolder * excep_holder
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException
    ));
};
#endif// BODY_AMI_UI_HANDLER_I_H
