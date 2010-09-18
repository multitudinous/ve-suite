/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#ifndef _VES_OPEN_XML_SHADER_SHADER_H_
#define _VES_OPEN_XML_SHADER_SHADER_H_

#include <ves/open/xml/shader/ShaderPtr.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/shader/TextureImagePtr.h>
#include <ves/open/xml/shader/UniformPtr.h>

#include <xercesc/dom/DOM.hpp>

#include <string>
#include <vector>
#include <map>


namespace ves
{
namespace open
{
namespace xml
{
namespace shader
{

/*!\file Shader.h
  Shader API
  */
/*!\class ves::open::xml::shader::Shader
 * Class that stores an data and information neccessary to create a glsl shader.
 */
class VE_SHADER_EXPORTS Shader: public ves::open::xml::XMLObject
{
public:
    ///Constructor
    Shader();

    ///Destructor
    virtual ~Shader();

    ///Copy constructor
    Shader( const Shader& rhs );

    ///Set the object from input XML data
    ///\param xmlInput The input xml data.
    void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Add a uniform variable to the shader
    ///\param newUniform The new uniform variable to add to the shader.
    void AddUniform( UniformPtr newUniform );

    ///Add a texture image to the shader.
    ///\todo This may not be necessary!!!!
    ///\param newTextureImage The texture image to add.
    void AddTextureImage( TextureImagePtr newTextureImage );

    ///The type of shader program. Valid types are "Vertex"
    ///and "Fragment".
    ///\param fragOrVert The type of shader this represents.
    void SetShaderType( const std::string& fragOrVert );

    ///This is the string containing the full source code
    ///of the shader. This includes the variables as well as the
    ///functions.
    ///\param shaderSourceCode The raw shader code.
    void SetShaderSource( const std::string& shaderSourceCode );

    ///Get the raw source for the shader.
    const std::string& GetShaderSource();

    ///Get the shader type.
    const std::string& GetShaderType();

    ///Get a texture representing an image file.
    ///\param textureUnit The texture unit to search for.
    const TextureImagePtr GetTextureImage( unsigned int textureUnit );

    ///Get the number of uniforms.
    size_t GetNumberOfUniforms();

    ///Get the number of texture images.
    size_t GetNumberOfTextureImages();

    ///Get a specific uniform by name.
    ///\param uniformName The uniform name to search for.
    const UniformPtr GetUniform( const std::string& uniformName );

    ///Get a specific uniform by index.
    ///\param index The uniform to search for.
    const UniformPtr GetUniform( unsigned int index );

    ///equal operator
    Shader& operator=( const Shader& rhs );
protected:
    ///Internally update the XML data for this element.
    ///\param input The XML element information
    virtual void _updateVEElement( const std::string& input );

    ///Internally update the texture images.
    void _updateTextureImages();

    ///Internally update the uniforms.
    void _updateUniforms();

    ///Internally update the shader type.
    void _updateShaderType();

    ///Internally update the shader source.
    void _updateShaderSource();

    std::string mShaderType;///<The type of shader represented.
    std::string mShaderSource;///<The raw shader source.

    std::map<unsigned int, TextureImagePtr> mTextureImages;///<The list of texture images.
    std::vector<UniformPtr> mUniformList;///<The list of uniforms.
};

}
}
}
}
#endif//SHADER_H
