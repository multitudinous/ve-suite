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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef SHADER_CREATOR_H
#define SHADER_CREATOR_H
/*!\file ShaderCreator.h
  ShaderCreator API
  */
/*!\class VE_Shader::ShaderCreator
 * Create Objects within the VE_Shader namespace.
 */

namespace VE_XML
{
   class XMLObject;
}
#include "VE_Open/XML/CreationEventHandler.h"
#include "VE_Installer/include/VEConfig.h"

namespace VE_Shader{
class VE_SHADER_EXPORTS ShaderCreator : public VE_XML::CreationEventHandler{
public:
   ///Constructor
   ShaderCreator(){}

   ///Destructor
   virtual ~ShaderCreator(){}

   ///Create a new XMLObject.
   ///\param objectType The type of object to create.
   virtual VE_XML::XMLObject* CreateNewXMLObject(std::string objectType);
   
   ///Create a copy of a new CAD object
   ///\param objectType The type of object to create.
   ///\param objectToCopy The object to copy.
   virtual VE_XML::XMLObject* CreateNewXMLObjectCopy(std::string objectType,VE_XML::XMLObject* objectToCopy);
protected:
};
}
#endif// SHADER_CREATOR_H


