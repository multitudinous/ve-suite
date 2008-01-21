/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef PROGRAM_H
#define PROGRAM_H

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/shader/ShaderPtr.h>
#include <ves/VEConfig.h>
#include <xercesc/dom/DOM.hpp>
#include <string>
#include <vector>

/*!\file Program.h
  Program API
  */
/*!\class VE_Shader::Program
 * Class that stores an data and information neccessary to create a glsl Program program.
 */

namespace ves
{
namespace open
{
namespace xml
{
namespace shader
{
class VE_SHADER_EXPORTS Program: public ves::open::xml::XMLObject
{
public:
    ///Constructor
    Program();

    ///Destructor
    virtual ~Program();

    ///Copy constructor
    Program( const Program& rhs );

    ///Set the vertex shader for this program
    ///\param vertShader The vertex shader.
    void SetVertexShader( ves::open::xml::shader::ShaderPtr vertShader );


    ///Set the fragment shader for this program
    ///\param fragShader The fragment shader.
    void SetFragmentShader( ves::open::xml::shader::ShaderPtr fragShader );

    ///Set the name of the glsl program
    ///\param name The name of the program.
    void SetProgramName( std::string name );

    ///Set the object from input XML data
    ///\param xmlInput The input xml data.
    void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the fragment shader.
    ves::open::xml::shader::ShaderPtr GetFragmentShader();

    ///Get the vertex shader.
    ves::open::xml::shader::ShaderPtr GetVertexShader();

    ///Get the name of the program.
    std::string GetProgramName();


    ///equal operator
    Program& operator=( const Program& rhs );
protected:
    ///Internally update the XML data for this element.
    ///\param input The XML element information
    virtual void _updateVEElement( std::string input );

    ///Internally update the name from the XML data.
    void _updateProgramName();

    std::string _name;///< The program name.
    ves::open::xml::shader::ShaderPtr _vertexShader;///< The vertex shader.
    ves::open::xml::shader::ShaderPtr _fragmentShader;///< The fragment shader.
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, ves::open::xml::shader::Program* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif //PROGRAM_H
