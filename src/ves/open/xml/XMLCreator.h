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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef XML_CREATOR_H
#define XML_CREATOR_H
/*!\file XMLCreator.h
  XMLCreator API
  */
/*!\class VE_XML::XMLCreator
 * Create XMLObject.
 */

#include "ves/open/xml/XMLObject.h"
#include "ves/open/xml/CreationEventHandler.h"
namespace VE_XML{
class VE_XML_EXPORTS XMLCreator: public CreationEventHandler{
public:
   ///Constructor
   XMLCreator(){}

   ///Destructor
   virtual ~XMLCreator(){}

   ///Create a new XMLObject.
   ///\param objectType The type of object to create.
   virtual VE_XML::XMLObject* CreateNewXMLObject(std::string objectType);

   ///Create a copy of a XMLObject
   ///\param objectType The type of object to create.
   ///\param objectToCopy The object to copy
   virtual VE_XML::XMLObject* CreateNewXMLObjectCopy(std::string objectType,
                                                  VE_XML::XMLObject* objectToCopy);
protected:
};
}
#endif// VE_CREATION_EVENT_HANDLER_H


