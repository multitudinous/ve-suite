/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef _VES_OPEN_XML_SHADER_PROGRAM_H_
#define _VES_OPEN_XML_SHADER_PROGRAM_H_

#include <ves/open/xml/shader/ProgramPtr.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/shader/ShaderPtr.h>

#include <xercesc/dom/DOM.hpp>

#include <string>
#include <vector>

namespace ves
{
namespace open
{
namespace xml
{
namespace shader
{

/*!\file Program.h
  Program API
  */
/*!\class ves::open::xml::shader::Program
 * Class that stores an data and information neccessary to create a glsl Program program.
 */
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
    void SetVertexShader( ShaderPtr vertShader );


    ///Set the fragment shader for this program
    ///\param fragShader The fragment shader.
    void SetFragmentShader( ShaderPtr fragShader );

    ///Set the name of the glsl program
    ///\param name The name of the program.
    void SetProgramName( const std::string& name );

    ///Set the object from input XML data
    ///\param xmlInput The input xml data.
    void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the fragment shader.
    ShaderPtr GetFragmentShader();

    ///Get the vertex shader.
    ShaderPtr GetVertexShader();

    ///Get the name of the program.
    const std::string& GetProgramName();


    ///equal operator
    Program& operator=( const Program& rhs );
protected:
    ///Internally update the XML data for this element.
    ///\param input The XML element information
    virtual void _updateVEElement( const std::string& input );

    ///Internally update the name from the XML data.
    void _updateProgramName();

    std::string mName;///< The program name.
    ShaderPtr mVertexShader;///< The vertex shader.
    ShaderPtr mFragmentShader;///< The fragment shader.
};

}
}
}
}
#endif //PROGRAM_H
