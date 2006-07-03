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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef VE_I_H_
#define VE_I_H_

#include "VE_Open/skel/moduleS.h"
#include "VE_Installer/include/VEConfig.h"
#include <string>
#include <vector>
#include <vpr/Sync/Mutex.h>

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */

namespace VE_Xplorer
{
//Class Body_UI_i
class  Body_UI_i : public virtual POA_Body::UI
{
public:
   //Constructor 
   Body_UI_i (Body::Executive_ptr exec, std::string name);

   //Destructor 
   virtual ~Body_UI_i (void);

   std::string UIName_;
   std::string GetNetworkString( void );
   std::string GetStatusString( void );
   bool GetNetworkFlag( void );

protected:
   Body::Executive_var executive_;
   std::vector< std::string > networkStringBuffer;
   std::vector< std::string > statusStringBuffer;
   vpr::Mutex stringBufferLock;  /**< A mutex to protect variables accesses */
   vpr::Mutex statusBufferLock;  /**< A mutex to protect variables accesses */
   void SetNetworkString( std::string tempString );

   virtual void UpdateNetwork (
      const char * network
      ACE_ENV_ARG_DECL
   )
   ACE_THROW_SPEC ((
      CORBA::SystemException
      , Error::EUnknown
   ));

   virtual void UpdateModuleUI (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
   )
   ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
   ));

   virtual void UpdateModuleResult (
      CORBA::Long module_id,
      const char * msg
      ACE_ENV_ARG_DECL
   )
   ACE_THROW_SPEC ((
      CORBA::SystemException
      , Error::EUnknown
   ));

   virtual void UpdateLinkContent (
    CORBA::Long id,
    const char * msg
    ACE_ENV_ARG_DECL
   )
   ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
   ));

   virtual void Raise (
    const char * notification
    ACE_ENV_ARG_DECL
   )
   ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
   ));
};
}
#endif
