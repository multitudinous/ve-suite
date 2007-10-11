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
#ifndef MODEL_CREATOR_H
#define MODEL_CREATOR_H
/*!\file ModelCreator.h
  ModelCreator API
  */
/*!\class VE_XML::VE_Model::ModelCreator
 * Create Model Objects.
 */

namespace VE_XML
{
   class XMLObject;
}
#include <ves/open/xml/CreationEventHandler.h>
#include <ves/open/xml/XMLObjectPtr.h>

namespace VE_XML
{
namespace VE_Model
{
class VE_MODEL_EXPORTS ModelCreator : public VE_XML::CreationEventHandler
{
public:
   ///Constructor
   ModelCreator(){ ; }

   ///Destructor
   virtual ~ModelCreator(){;}

   ///Create a new XMLObject.
   ///\param objectType The type of object to create.
   virtual VE_XML::XMLObject* CreateNewXMLObject( std::string objectType );
    ///Create a new XMLObject.
    ///\param objectType The type of object to create.
    virtual VE_XML::XMLObjectPtr CreateNewXMLObjectSmart( std::string objectType );
    
   ///Create a copy of a new CAD object
   ///\param objectType The type of object to create.
   ///\param objectToCopy The object to copy.
   virtual VE_XML::XMLObject* CreateNewXMLObjectCopy(std::string objectType,VE_XML::XMLObject* objectToCopy);
   ///Create a copy of a new CAD object
   ///\param objectType The type of object to create.
   ///\param objectToCopy The object to copy.
   virtual VE_XML::XMLObjectPtr CreateNewXMLObjectCopySmart( std::string objectType, VE_XML::XMLObjectPtr objectToCopy);
protected:
};
}
}

#endif// MODEL_CREATOR_H


