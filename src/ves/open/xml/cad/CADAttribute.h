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
#ifndef _CAD_ATTRIBUTE_H_
#define _CAD_ATTRIBUTE_H_

#include <ves/open/xml/XMLObject.h>
#include <xercesc/dom/DOM.hpp>
#include <string>

/*!\file CADAttribute.h
  CADAttribute API
  */
/*!\class VE_XML::VE_CAD::CADAttribute
 * This class holds data for describing attributes for a CADNode.
 */

namespace ves
{
namespace open
{
namespace xml
{
namespace shader
{
   class Program;
}
}
}
}

namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class CADMaterial;

class VE_CAD_EXPORTS CADAttribute: public ves::open::xml::XMLObject
{
public:
   ///Constructor
   CADAttribute();
   virtual ~CADAttribute();

   ///Set the type of attribute.
   ///Valid types are "Material" or "Program".
   ///\param attributeType The type of attribute.
   void SetAttributeType(std::string attributeType);

   ///Set the material for this node.
   ///\param material The material of this node.
   void SetMaterial(ves::open::xml::cad::CADMaterial material);

   ///Set the GLSL program for this node.
   ///\param glslProgram The GLSL program.
   void SetProgram( ves::open::xml::shader::Program glslProgram);

   ///Set the object from XML data
   ///\param xmlNode Node to set this object from
   virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlNode);

   ///Enable blending for this attribute. 
   void EnableBlending();

   ///Disable the blending for this attribute.
   void DisableBlending();

   ///Check for blending
   bool NeedsBlending();

   ///Get the attribute type. 
   std::string GetAttributeType();

   ///Get the material of this CAD node
   ves::open::xml::cad::CADMaterial* GetMaterial();
  
   ///Get the GLSL program for this node.
   ves::open::xml::shader::Program* GetGLSLProgram();

   ///Get the name of a specific attribute.
   std::string GetAttributeName();

   ///Copy constructor
   CADAttribute(const CADAttribute& rhs);

   ///Equal operator
   CADAttribute& operator=(const CADAttribute& rhs);

protected:
   
   ///Internally update the XML data for this element.
   ///\param input The XML element information
   virtual void _updateVEElement(std::string input);

   std::string _attributeType;///<The type of attribute

   bool _blending;///<Enable or disable blending;

   ves::open::xml::cad::CADMaterial* _material; ///< Material for this node.
   ves::open::xml::shader::Program* _glslProgram;///<The glsl program.
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, ves::open::xml::cad::CADAttribute* val)
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// _CAD_ATTRIBUTE_H_
