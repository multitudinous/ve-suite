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

#ifndef _VE_XML_OBJECT_FACTORY_H_
#define _VE_XML_OBJECT_FACTORY_H_

#include <iostream>
#include <string>
#include <map>

#include <ves/VEConfig.h>
/*!\file XMLObjectFactory.h
  Factory class for creating XMLObject 
  */
/*!\class VE_XML::XMLObjectFactory
  *Factory class for creating XMLObject s
 */
namespace ves
{
namespace open
{
namespace xml
{
   class XMLObject;
   class CreationEventHandler;
}
}
}

namespace ves
{
namespace open
{
namespace xml
{
class VE_XML_EXPORTS XMLObjectFactory
{
public:

   ///Get an instance of the ObjectFactory
   static XMLObjectFactory* Instance();
   
   ///Cleanup the instance of the ObjectFactory
   static void DeleteInstance();

   ///\param objectType The unique string specifying what object to create
   ///\param objectNamespace The namespace that the object belongs to. 
   XMLObject* CreateXMLObject(std::string objectType,
                                             std::string objectNamespace);

   ///\param objectToCopy The object to pass to the copy constructor
   XMLObject* CreateXMLObjectCopy( XMLObject* objectToCopy );

   ///Register a CreationEventHandler for a namespace
   ///Valid types so far are:
   ///XML == Objects from the VE_XML namespace
   ///CAD == Objects from the CAD namespace
   ///Shader == Objects from the Shader
   ///\param objectNamespace The unique string specifying what namespace to register
   ///\param objectCreator The objectCreator to register 
   bool RegisterObjectCreator(std::string objectNamespace,CreationEventHandler* objectCreator);

   ///Check if a CreationEventHandler is registered 
   ///Valid types so far are:
   ///XML == Objects from the VE_XML namespace
   ///CAD == Objects from the CAD namespace
   ///Shader == Objects from the Shader
   ///\param objectNamespace The unique string specifying what namespace to register
   bool ObjectCreatorIsRegistered(std::string objectNamespace);
protected:
   ///Base constructor
   XMLObjectFactory( );
   ///Destructor
   virtual ~XMLObjectFactory();

   static XMLObjectFactory* _instanceOfFactory;///<The instance of the XMLObjectFactory
   std::map< std::string,CreationEventHandler*> _objectCreators;///<Creators of XMLObjects in different namespaces.
};
}
}
}
#endif// _VE_XML_OBJECT_FACTORY_H_
