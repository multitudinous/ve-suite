/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
#ifndef SHADER_HELPER_H
#define SHADER_HELPER_H
/*!\file ShaderHelper.h
  ShaderHelper API
  */
/*!\class ves::xplorer::scenegraph::util::ShaderHelper
 * Class that creates an OSG StateSet representing
 * a glsl program.
 */
#include <ves/VEConfig.h>
#include <ves/open/xml/shader/ShaderPtr.h>
///\todo This class still needs to be implemented for performer
#ifdef _OSG
#include <osg/StateSet>

namespace osg
{
class Shader;
class Program;
}
#include <osg/Texture>
#elif _PERFORMER
#endif
#include <ves/open/xml/shader/TextureImagePtr.h>
#include <ves/open/xml/shader/ProgramPtr.h>
#include <ves/open/xml/shader/UniformPtr.h>
#include <string>
#include <vector>
///////////////////////////////////////////////////////////////////////
//this class is used to create a stateset representing a glsl program//
///////////////////////////////////////////////////////////////////////
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace util
{
class VE_SCENEGRAPH_UTILS_EXPORTS ShaderHelper
{
public:
    ///Constructor
    ShaderHelper();

    ///Copy Constructor
    ShaderHelper( const ShaderHelper& rhs );

    ///Destructor
    virtual ~ShaderHelper();

    ///Load and create the stateset from the input XML data
    void LoadGLSLProgram( ves::open::xml::shader::ProgramPtr glslProgram );

    ///Load and create the stateset for transparency shader
    void LoadTransparencyProgram();

    ///Get the created state set representing the shader
    osg::StateSet* GetProgramStateSet();

    ///The state set that we want to load the shader into
    ///\param shader The state set representing the shader.
    void SetStateSet( osg::StateSet* shader );

    ///Update a uniform.
    ///\param The uniform to update.
    void UpdateUniform( ves::open::xml::shader::UniformPtr uniformToUpdate );

    ///Set wether to use two sided lighting
    void SetTwoSidedLighting( bool twoSided );
    
    ///Equal operator
    ///\param rhs Right hand side.
    ShaderHelper& operator=( const ShaderHelper& rhs );
protected:
    ///helper functions
    ///Utility function to create a shader.
    ///\param shaderData The XML shader data.
    void _createGLSLShader( ves::open::xml::shader::ShaderPtr shaderData );

    ///Attach the program to the stateset
    ///\param override Flag to override the stateset above
    void _attachGLSLProgramToStateSet( bool override = false );

    ///Extract uniforms from the shader.
    ///\param The shader to extract uniforms from.
    void _extractUniformsFromShader( ves::open::xml::shader::ShaderPtr shader );

    ///Extract the texture images from the shader information.
    ///\param textureImage The texture image data.
    void _extractTextureFromShader( ves::open::xml::shader::TextureImagePtr textureImage );

    ///Extract the wrap modes for the texture images
    ///\param texture The texture.
    ///\param param The wrap parameter
    ///\param wrapMode The wrap mode.
    void _setWrapOnTexture( osg::Texture* texture,
                            osg::Texture::WrapParameter param,
                            std::string wrapMode );

    std::vector<std::string> m_vertexUniformNames;///<Vertex program uniform names.
    std::vector<std::string> m_fragmentUniformNames;///<Fragment program uniform names.
    osg::ref_ptr<osg::Shader> m_vshader;///<The vertex shader.
    osg::ref_ptr<osg::Shader> m_fshader;///<The fragment shader.
    osg::ref_ptr<osg::Program> m_glslProgram;///<The GLSL program.
    osg::ref_ptr<osg::StateSet> m_ss;///<The stateset representing the GLSL program.
    bool m_twoSidedLighting;
};
}
}
}
}
#endif// SHADER_HELPER_H
