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
#ifndef VE_PROGRAM_H
#define VE_PROGRAM_H

#include "ves/open/xml/XMLObject.h"
#include "ves/open/xml/Shader/ShaderPtr.h"
#include "VE_Installer/include/VEConfig.h"
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>

/*!\file Program.h
  Program API 
  */
/*!\class VE_Shader::Program
 * Class that stores an data and information neccessary to create a glsl Program program.
 */

namespace VE_XML
{
namespace VE_Shader
{
class VE_SHADER_EXPORTS Program:public VE_XML::XMLObject{
public:
   ///Constructor
   Program();

   ///Destructor
   virtual ~Program();
  
   ///Copy constructor
   Program(const Program& rhs);

   ///Set the vertex shader for this program
   ///\param vertShader The vertex shader.
   void SetVertexShader(VE_Shader::ShaderPtr vertShader);

   
   ///Set the fragment shader for this program
   ///\param fragShader The fragment shader.
   void SetFragmentShader(VE_Shader::ShaderPtr fragShader);

   ///Set the name of the glsl program
   ///\param name The name of the program.
   void SetProgramName(std::string name);

   ///Set the object from input XML data
   ///\param xmlInput The input xml data.
   void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput);

   ///Get the fragment shader.
   VE_Shader::ShaderPtr GetFragmentShader();

   ///Get the vertex shader.
   VE_Shader::ShaderPtr GetVertexShader();

   ///Get the name of the program.
   std::string GetProgramName();

   
   ///equal operator
   Program& operator=(const Program& rhs);
protected:
   ///Internally update the XML data for this element.
   ///\param input The XML element information
   virtual void _updateVEElement(std::string input);

   ///Internally update the name from the XML data.
   void _updateProgramName();

   std::string _name;///< The program name.
   VE_Shader::ShaderPtr _vertexShader;///< The vertex shader.
   VE_Shader::ShaderPtr _fragmentShader;///< The fragment shader.
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(const std::string subElementTagName, VE_XML::VE_Shader::Program* val)
{
   val->SetOwnerDocument( _rootDocument );
   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
   _veElement->appendChild( childElement );
   return childElement;
}
}
#endif //VE_PROGRAM_H
