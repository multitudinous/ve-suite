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
 * File:          $RCSfile: VEUser.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _XML_VE_USER_H_
#define _XML_VE_USER_H_
/*!\file VEUser.h
  User description.
  */
/*!\class VE_XML::VEUser
 * This class manages information describing a user.
 */
namespace VE_XML{
   class VEStateInfo;
}

#include <xercesc/dom/DOM.hpp>

#include <iostream>

#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/VE_XML/VEXMLObject.h"
namespace VE_XML
{
class VE_XML_EXPORTS VEUser : public VEXMLObject
{
public:
   ///Constructor
   ///\param rootDoc The owning DOMDocument.
   VEUser(DOMDocument* rootDoc);
   ///Destructor
   virtual ~VEUser();
   ///The control status of this user
   ///Valid status
   ///MASTER == controlling changes of the main graphics state
   ///SLAVE == observing changes of the main graphics state
   /*\fn typedef VEControlStatus 
    *The control status of the user.
    */
   typedef std::string VEControlStatus;
   
   ///Set the users ID
   ///\param id String to uniquely identify the user
   void SetUserId(std::string id);
   ///Set the control status of the user
   ///\param cs The control status.
   void SetControlStatus(VEControlStatus cs);
   ///Set the state information for this user
   ///\param userState The VEStateInfo for this user.
   void SetStateInfo(VE_XML::VEStateInfo* userState);

   ///Return the user id
   std::string GetUserId();
   ///Return the VEControlStatus of this user
   VEControlStatus GetControlStatus();
   ///Return the VEStateInfo for this user.
   VE_XML::VEStateInfo* GetUserStateInfo();

   ///Set the data for this object from an XML element
   ///\param xmlInput The input XML element
  virtual void SetObjectFromXMLData(DOMNode* xmlInput);   

protected:
   ///Internally update this element
   ///\param tagName The tagName for this element.
   virtual void _updateVEElement( std::string tagName );
   std::string _userId;///<The users unique identification
   VE_XML::VEStateInfo* _stateInfo;///<The VEStateInfo for this user.
   VEControlStatus _controlStatus;///<The VEControlStatus of this user.
};
}
#endif// _XML_VE_USER_H_
