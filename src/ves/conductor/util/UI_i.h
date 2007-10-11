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
#ifndef UI_I_H_
#define UI_I_H_
/*!\file UI_i.h
Body_UI_i API
*/
/*!\class Body_UI_i
* 
*/
#include <ves/open/skel/moduleS.h>
#include <iostream>
#include <string>
#include <vector>
#include <map>

#include <ves/open/xml/Command.h>

class PEThread;
//class Network;

namespace VE_XML
{
	class XMLObject;
    class Command;
	namespace VE_Model
	{
		class Model;
	}
}

//Class Body_UI_i
#include <ves/VEConfig.h>
class VE_CONDUCTOR_UTILS_EXPORTS Body_UI_i : public virtual POA_Body::UI
{
 public:
  //Constructor 
  Body_UI_i (Body::Executive_ptr exec, std::string name);
  
  //Destructor 
  virtual ~Body_UI_i (void);
  
  std::string UIName_;

 protected:
  Body::Executive_var executive_;
  PEThread* logWindow;
 public:

     void SetLogWindow( PEThread* logWindow );

	 ///Returns vector containing XMLObjects
	 VE_XML::Command GetXplorerData( std::string commandName );
     
virtual void UpdateNetwork (
    const char * network
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void UpdateModuleUI (
    CORBA::Long module_id,
    const char * msg
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void UpdateModuleResult (
    CORBA::Long module_id,
    const char * msg
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void UpdateLinkContent (
    CORBA::Long id,
    const char * msg
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void Raise (
    const char * notification
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

  virtual
  void SetXplorerData (
      const char * xplorerData
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

  std::map<int,std::string> m_idToModelName;
  std::map<std::string, VE_XML::XMLObject*> m_objectToModel;
  std::map< std::string, VE_XML::Command > m_commandNameMap;
};


#endif
