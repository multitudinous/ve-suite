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

#include <ves/xplorer/scenegraph/util/ShaderHelper.h>
#include <ves/xplorer/scenegraph/util/PerlinNoiseTexture.h>
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Shader>
#include <osg/Uniform>
#include <osg/Texture>
#include <osg/Texture1D>
#include <osg/Texture2D>
#include <osg/Texture3D>
#include <osg/TextureCubeMap>
#include <osg/TexGen>
#include <osgDB/ReadFile>
#elif _PEFORMER
#endif

#include <iostream>
#include <sstream>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/shader/Program.h>
#include <ves/open/xml/shader/Uniform.h>
#include <ves/open/xml/shader/TextureImage.h>

using namespace ves::open::xml::shader;
using namespace ves::xplorer::scenegraph::util;
//////////////////////////////////////
//Constructors                      //
//////////////////////////////////////
ShaderHelper::ShaderHelper()
{}

///////////////////////////////////////////////////
ShaderHelper::ShaderHelper( const ShaderHelper& rhs )
{
    for( size_t i = 0; i < rhs.m_vertexUniformNames.size(); i++ )
    {
        m_vertexUniformNames.push_back( rhs.m_vertexUniformNames.at( i ) );
    }
    for( size_t i = 0; i < rhs.m_fragmentUniformNames.size(); i++ )
    {
        m_fragmentUniformNames.push_back( rhs.m_fragmentUniformNames.at( i ) );
    }
#ifdef _OSG
    if( rhs.m_vshader.valid() )
        m_vshader = new osg::Shader( *rhs.m_vshader.get() );

    if( rhs.m_fshader.valid() )
        m_fshader = new osg::Shader( *rhs.m_fshader.get() );

    m_glslProgram = new osg::Program( *m_glslProgram.get() );
    m_ss = new osg::StateSet( *rhs.m_ss );
#elif _PERFORMER
#endif
}
//////////////////////////////////////////////////////////////
ShaderHelper& ShaderHelper::operator=( const ShaderHelper& rhs )
{
    if( this != &rhs )
    {
        m_vertexUniformNames.clear();
        m_fragmentUniformNames.clear();
        for( size_t i = 0; i < rhs.m_vertexUniformNames.size(); i++ )
        {
            m_vertexUniformNames.push_back( rhs.m_vertexUniformNames.at( i ) );
        }
        for( size_t i = 0; i < rhs.m_fragmentUniformNames.size(); i++ )
        {
            m_fragmentUniformNames.push_back( rhs.m_fragmentUniformNames.at( i ) );
        }
#ifdef _OSG

        if( rhs.m_vshader.valid() )
            m_vshader = rhs.m_vshader;

        if( rhs.m_fshader.valid() )
            m_fshader = rhs.m_fshader;

        m_glslProgram = rhs.m_glslProgram;
        m_ss = rhs.m_ss;
#elif _PERFORMER
#endif
    }
    return *this;
}
///////////////////////////////////////////
//Destructor                             //
///////////////////////////////////////////
ShaderHelper::~ShaderHelper()
{
    m_vertexUniformNames.clear();
    m_fragmentUniformNames.clear();
}
#ifdef _OSG
/////////////////////////////////////////////////////
void ShaderHelper::SetStateSet( osg::StateSet* shader )
{
    m_ss = shader;
}
#elif _PERFORMER
#endif
////////////////////////////////////////////
void ShaderHelper::LoadTransparencyProgram()
{
#ifdef _OSG
    ShaderPtr vertShader = new Shader();
    vertShader->SetShaderType( "Vertex" );
    std::string vertexSource( " varying vec3 N;\n"
                              "varying vec3 I;\n"
                              "varying vec4 currentColor;\n"

                              "void main()\n"
                              "{\n"
                              "vec4 P=gl_ModelViewMatrix*gl_Vertex;\n"
                              "I=P.xyz;\n"
                              "N=gl_NormalMatrix*gl_Normal;\n"
                              "currentColor=gl_Color;\n"
                              "gl_Position=ftransform();\n"
                              "}\n" );
    vertShader->SetShaderSource( vertexSource );

    ShaderPtr fragShader = new Shader();
    fragShader->SetShaderType( "Fragment" );
    std::string fragmentSource( "varying vec3 N;\n"
                                " varying vec3 I;\n"
                                "varying vec4 currentColor;\n"
                                "void main()\n"
                                "{\n"
                                "float opac=dot(normalize(-N),normalize(-I));\n"
                                "opac=abs(opac);\n"
                                "opac=1.0-pow(opac,0.8);\n"
                                "vec4 Cs=currentColor;\n"
                                "gl_FragColor=opac*Cs;\n"
                                "}\n"
                              );
    fragShader->SetShaderSource( fragmentSource );

    ProgramPtr glslProgram = new Program();
    glslProgram->SetProgramName( "Dataset Transparency" );
    glslProgram->SetVertexShader( vertShader );
    glslProgram->SetFragmentShader( fragShader );
    LoadGLSLProgram( glslProgram );
#elif _PERFORMER
#endif
}
///////////////////////////////////////////////////////////////////
void ShaderHelper::LoadGLSLProgram( ProgramPtr glslProgram )
{
#ifdef _OSG
    //std::cout<<"Loading GLSLProgram: "<<glslProgram->GetProgramName()<<std::endl;
    if( !m_ss.valid() )
    {
        m_ss = new osg::StateSet();
    }
    else
    {
        m_ss->clear();
    }

    if( !m_glslProgram.valid() )
    {
        m_glslProgram = new osg::Program();
    }
    m_glslProgram->setName( glslProgram->GetProgramName() );

    if( glslProgram->GetFragmentShader() )
    {
        _createGLSLShader( glslProgram->GetFragmentShader() );
    }
    if( glslProgram->GetVertexShader() )
    {
        _createGLSLShader( glslProgram->GetVertexShader() );
    }
    ///two-sided lighting hack until gl_FrontFacing works in glsl...
    ///only works if the shader implements it though...
    m_ss->setMode( GL_VERTEX_PROGRAM_TWO_SIDE, osg::StateAttribute::ON );
#elif _PERFORMER
    std::cout << "Not implemented for Performer yet!!!" << std::endl;
#endif
    _attachGLSLProgramToStateSet();
}
///////////////////////////////////////////////////////////////
void ShaderHelper::_createGLSLShader( ShaderPtr shader )
{
    if( shader->GetShaderSource().empty() )
    {
        std::cout << "Couldn't load shader from inline source because the source is empty!" << std::endl;
        return;
    }
    _extractUniformsFromShader( shader );
#ifdef _OSG
    if( shader->GetShaderType() == std::string( "Fragment" ) )
    {
        if( !m_fshader )
        {
            m_fshader = new osg::Shader( osg::Shader::FRAGMENT, shader->GetShaderSource() );
        }
        else
        {
            m_fshader->setShaderSource( shader->GetShaderSource() );
        }
        m_glslProgram->addShader( m_fshader.get() );
    }
    else if( shader->GetShaderType() == std::string( "Vertex" ) )
    {
        if( !m_vshader )
        {
            m_vshader = new osg::Shader( osg::Shader::VERTEX, shader->GetShaderSource() );
        }
        else
        {
            m_vshader->setShaderSource( shader->GetShaderSource() );
        }
        m_glslProgram->addShader( m_vshader.get() );
    }
#elif _PERFORMER
    std::cout << "Not implemented for Performer yet!!!" << std::endl;
#endif
}
///////////////////////////////////////////////////////////////////////////////////
void ShaderHelper::_extractTextureFromShader( TextureImagePtr textureImage )
{
    //create the image
    unsigned int tUnit = textureImage->GetTextureUnit();
    unsigned int dimension = textureImage->GetDimension();

    //std::cout<<"Reading image file: "<<std::endl;
    osg::ref_ptr<osg::Image> textureImageData = osgDB::readImageFile( textureImage->GetImageFile() );
    //std::cout<<"Read image file: "<<std::endl;
    osg::ref_ptr<osg::Texture> genericTexture;
    std::string textureType( "" );
    textureImage->GetType( textureType );

    //std::cout<<"Extracting: "<<textureType<<std::endl;
    if( textureType == "1D" )
    {
        osg::ref_ptr<osg::Texture1D> texture1D = new osg::Texture1D();
        //we need to set the wrapping and filters still!!!!
        texture1D->setImage( textureImageData.get() );

        //Set the texture unit on the state set
        genericTexture = texture1D.get();
    }
    else if( textureType == "2D" )
    {
        osg::ref_ptr<osg::Texture2D> texture2D = new osg::Texture2D();
        //we need to set the wrapping and filters still!!!!
        texture2D->setImage( textureImageData.get() );
        genericTexture = texture2D.get();
    }
    else if( textureType == "3D" )
    {
        osg::ref_ptr<osg::Texture3D> texture3D = new osg::Texture3D();
        //we need to set the wrapping and filters still!!!!
        texture3D->setImage( textureImageData.get() );
        genericTexture = texture3D.get();
    }
    else if( textureType == "Cube" )
    {
        //std::cout<<"Cube map"<<std::endl;
        osg::ref_ptr<osg::TextureCubeMap> textureCubeMap = new osg::TextureCubeMap();
        textureCubeMap->setImage( osg::TextureCubeMap::POSITIVE_X, osgDB::readImageFile( textureImage->GetImageFile( "Positive X" ) ) );
        textureCubeMap->setImage( osg::TextureCubeMap::NEGATIVE_X, osgDB::readImageFile( textureImage->GetImageFile( "Negative X" ) ) );
        textureCubeMap->setImage( osg::TextureCubeMap::POSITIVE_Y, osgDB::readImageFile( textureImage->GetImageFile( "Positive Y" ) ) );
        textureCubeMap->setImage( osg::TextureCubeMap::NEGATIVE_Y, osgDB::readImageFile( textureImage->GetImageFile( "Negative Y" ) ) );
        textureCubeMap->setImage( osg::TextureCubeMap::POSITIVE_Z, osgDB::readImageFile( textureImage->GetImageFile( "Positive Z" ) ) );
        textureCubeMap->setImage( osg::TextureCubeMap::NEGATIVE_Z, osgDB::readImageFile( textureImage->GetImageFile( "Negative Z" ) ) );

        //this should be adjustable parameter in the TextureImage interface
        osg::ref_ptr<osg::TexGen> textureCoordGeneration = new osg::TexGen;
        textureCoordGeneration->setMode( osg::TexGen::REFLECTION_MAP );
        m_ss->setTextureAttributeAndModes( tUnit, textureCoordGeneration.get(), osg::StateAttribute::ON );

        genericTexture = textureCubeMap.get();
    }
    else if( textureType == "Perlin Noise" )
    {
        PerlinNoiseTexture perlinNoise( 64, 64, 64 );
        osg::ref_ptr<osg::Texture3D> texture3D =
            dynamic_cast<osg::Texture3D*>( perlinNoise.GetNoiseTexture() );
        genericTexture = texture3D.get();
    }

    if( genericTexture.valid() )
    {
        //std::cout<<"Setting up texture parameters for shader!"<<std::endl;
        std::string minFilter;
        textureImage->GetFilterMode( "Minification", minFilter );

        std::string magFilter;
        textureImage->GetFilterMode( "Magnification", magFilter );

        osg::Texture::FilterMode magMode = osg::Texture::LINEAR;
        osg::Texture::FilterMode minMode = osg::Texture::LINEAR;

        if( minFilter == "Nearest" )
        {
            minMode =  osg::Texture::NEAREST;
        }

        if( magFilter == "Nearest" )
        {
            magMode = osg::Texture::NEAREST;
        }
        //set the min/mag filters
        genericTexture->setFilter( osg::Texture::MAG_FILTER, magMode );
        genericTexture->setFilter( osg::Texture::MIN_FILTER, minMode );

        //set the wrap modes
        std::string sWrap;
        std::string tWrap;
        std::string rWrap;

        osg::Texture::WrapMode swrapMode = osg::Texture::CLAMP;

        if( textureImage->GetWrapMode( "Wrap S", sWrap ) )
            _setWrapOnTexture( genericTexture.get(), osg::Texture::WRAP_S, sWrap );

        if( dimension != 1 )
        {
            if( textureImage->GetWrapMode( "Wrap T", tWrap ) )
                _setWrapOnTexture( genericTexture.get(), osg::Texture::WRAP_T, tWrap );
        }

        if( dimension == 3 )
        {
            if( textureImage->GetWrapMode( "Wrap R", rWrap ) )
                _setWrapOnTexture( genericTexture.get(), osg::Texture::WRAP_R, rWrap );
        }

        //std::cout<<"Is this the problem??"<<std::endl;
        //set the texture to the state set
        m_ss->setTextureAttributeAndModes( tUnit, genericTexture.get(), osg::StateAttribute::ON );

        if( dimension == 1 )
            m_ss->setTextureMode( tUnit, GL_TEXTURE_1D, osg::StateAttribute::ON );

        if( dimension == 2 )
            m_ss->setTextureMode( tUnit, GL_TEXTURE_2D, osg::StateAttribute::ON );

        if( dimension == 3 )
        {
            if( textureType == "3D" )
            {
                //std::cout<<"Probably"<<std::endl;
                m_ss->setTextureMode( tUnit, GL_TEXTURE_3D, osg::StateAttribute::ON );
            }
        }
    }
}
///////////////////////////////////////////////////////////////////////
void ShaderHelper::_setWrapOnTexture( osg::Texture* texture,
                                      osg::Texture::WrapParameter param,
                                      std::string wrapMode )
{
    if( wrapMode == "Clamp to Border" )
    {
        texture->setWrap( param, osg::Texture::CLAMP_TO_BORDER );
    }
    else if( wrapMode == "Clamp to Edge" )
    {
        texture->setWrap( param, osg::Texture::CLAMP_TO_EDGE );
    }
    else if( wrapMode == "Repeat" )
    {
        texture->setWrap( param, osg::Texture::REPEAT );
    }
    else if( wrapMode == "Mirror" )
    {
        texture->setWrap( param, osg::Texture::MIRROR );
    }
    else if( wrapMode == "Clamp" )
    {
        texture->setWrap( param, osg::Texture::CLAMP );
    }
}
/////////////////////////////////////////////////////////////////
void ShaderHelper::UpdateUniform( UniformPtr uniformData )
{
    std::string uniformName( "" );
    std::string uniformType( "" );
    unsigned int uniformSize = 0;
    std::vector<float> uniformValues;

    uniformName = uniformData->GetName();
    uniformType = uniformData->GetType();
    uniformSize = uniformData->GetSize();
    uniformValues = uniformData->GetValues();

    osg::ref_ptr<osg::Uniform> uniformToUpdate = m_ss->getUniform( uniformName );
    if( uniformToUpdate.valid() )
    {
        if( uniformType == "Float" )
        {
            if( uniformSize == 1 )
            {
                uniformToUpdate->set( uniformValues.at( 0 ) );
            }
            else if( uniformSize == 2 )
            {
                uniformToUpdate->set( osg::Vec2f( uniformValues.at( 0 ), uniformValues.at( 0 ) ) );
            }
            else if( uniformSize == 3 )
            {
                uniformToUpdate->set( osg::Vec3f( uniformValues.at( 0 ),
                                                  uniformValues.at( 1 ),
                                                  uniformValues.at( 2 ) ) );
            }
            else if( uniformSize == 4 )
            {
                uniformToUpdate->set( osg::Vec4f( uniformValues.at( 0 ),
                                                  uniformValues.at( 1 ),
                                                  uniformValues.at( 2 ),
                                                  uniformValues.at( 3 ) ) );
            }
        }
        else if( uniformType == "Int" )
        {
            if( uniformSize == 1 )
            {
                uniformToUpdate->set( static_cast<int>( uniformValues.at( 0 ) ) );
            }
            else if( uniformSize == 2 )
            {
                uniformToUpdate->set( osg::Vec2( static_cast<int>( uniformValues.at( 0 ) ),
                                                 static_cast<int>( uniformValues.at( 1 ) ) ) );
            }
            else if( uniformSize == 3 )
            {
                uniformToUpdate->set( osg::Vec3( static_cast<int>( uniformValues.at( 0 ) ),
                                                 static_cast<int>( uniformValues.at( 1 ) ),
                                                 static_cast<int>( uniformValues.at( 2 ) ) ) );
            }
            else if( uniformSize == 4 )
            {
                uniformToUpdate->set( osg::Vec4( static_cast<int>( uniformValues.at( 0 ) ),
                                                 static_cast<int>( uniformValues.at( 1 ) ),
                                                 static_cast<int>( uniformValues.at( 2 ) ),
                                                 static_cast<int>( uniformValues.at( 3 ) ) ) );
            }
        }
        else if( uniformType == "Bool" )
        {
            std::vector<bool> boolValues;
            for( size_t i = 0; i < uniformSize; i++ )
            {
                if( uniformValues.at( i ) == 0.0 )
                {
                    boolValues.push_back( false );
                }
                else
                {
                    boolValues.push_back( true );
                }
            }
            if( uniformSize == 1 )
            {
                uniformToUpdate->set( boolValues.at( 0 ) );
            }
            else if( uniformSize == 2 )
            {
                uniformToUpdate->set( osg::Vec2( boolValues.at( 0 ),
                                                 boolValues.at( 1 ) ) );
            }
            else if( uniformSize == 3 )
            {
                uniformToUpdate->set( osg::Vec3( boolValues.at( 0 ),
                                                 boolValues.at( 1 ),
                                                 boolValues.at( 2 ) ) );
            }
            else if( uniformSize == 4 )
            {
                uniformToUpdate->set( osg::Vec4( boolValues.at( 0 ),
                                                 boolValues.at( 1 ),
                                                 boolValues.at( 2 ),
                                                 boolValues.at( 3 ) ) );
            }
        }
        else if( uniformType == "Sampler" )
        {
            std::cout << "Updating Sampler!!" << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////
void ShaderHelper::_extractUniformsFromShader( ShaderPtr shader )
{
    size_t nUniforms = shader->GetNumberOfUniforms();
    UniformPtr uniformData;
    std::string uniformName( "" );
    std::string uniformType( "" );
    unsigned int uniformSize = 0;
    std::vector<float> uniformValues;
    for( size_t i = 0; i < nUniforms; i++ )
    {
        uniformData = shader->GetUniform( i );
        uniformName = uniformData->GetName();
        uniformType = uniformData->GetType();
        uniformSize = uniformData->GetSize();
        uniformValues = uniformData->GetValues();

        if( uniformType == "Float" )
        {
            if( uniformSize == 1 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), uniformValues.at( 0 ) ) );
            }
            else if( uniformSize == 2 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec2f( uniformValues.at( 0 ), uniformValues.at( 0 ) ) ) );
            }
            else if( uniformSize == 3 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec3f( uniformValues.at( 0 ),
                                                   uniformValues.at( 1 ),
                                                   uniformValues.at( 2 ) ) ) );
            }
            else if( uniformSize == 4 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec4f( uniformValues.at( 0 ),
                                                   uniformValues.at( 1 ),
                                                   uniformValues.at( 2 ),
                                                   uniformValues.at( 3 ) ) ) );
            }
        }
        else if( uniformType == "Int" )
        {
            if( uniformSize == 1 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), static_cast<int>( uniformValues.at( 0 ) ) ) );
            }
            else if( uniformSize == 2 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec2( static_cast<int>( uniformValues.at( 0 ) ),
                                                   static_cast<int>( uniformValues.at( 1 ) ) ) ) );
            }
            else if( uniformSize == 3 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec3( static_cast<int>( uniformValues.at( 0 ) ),
                                                   static_cast<int>( uniformValues.at( 1 ) ),
                                                   static_cast<int>( uniformValues.at( 2 ) ) ) ) );
            }
            else if( uniformSize == 4 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec4( static_cast<int>( uniformValues.at( 0 ) ),
                                                   static_cast<int>( uniformValues.at( 1 ) ),
                                                   static_cast<int>( uniformValues.at( 2 ) ),
                                                   static_cast<int>( uniformValues.at( 3 ) ) ) ) );
            }
        }
        else if( uniformType == "Bool" )
        {
            std::vector<bool> boolValues;
            for( size_t i = 0; i < uniformSize; i++ )
            {
                if( uniformValues.at( i ) == 0.0 )
                {
                    boolValues.push_back( false );
                }
                else
                {
                    boolValues.push_back( true );
                }
            }
            if( uniformSize == 1 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), boolValues.at( 0 ) ) );
            }
            else if( uniformSize == 2 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec2( boolValues.at( 0 ),
                                                   boolValues.at( 1 ) ) ) );
            }
            else if( uniformSize == 3 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec3( boolValues.at( 0 ),
                                                   boolValues.at( 1 ),
                                                   boolValues.at( 2 ) ) ) );
            }
            else if( uniformSize == 4 )
            {
                m_ss->addUniform( new osg::Uniform( uniformName.c_str(), osg::Vec4( boolValues.at( 0 ),
                                                   boolValues.at( 1 ),
                                                   boolValues.at( 2 ),
                                                   boolValues.at( 3 ) ) ) );
            }
        }
        else if( uniformType == "Sampler" )
        {
            std::cout << "Extracting Sampler!!" << std::endl;
            std::cout << "Unit: " << uniformData->GetTextureUnit() << std::endl;

            _extractTextureFromShader( shader->GetTextureImage( uniformData->GetTextureUnit() ) );
            std::cout << "---Done---" << std::endl;
        }
    }
}
/////////////////////////////////////////////////////////////////
void ShaderHelper::_attachGLSLProgramToStateSet( bool override )
{
#ifdef _OSG
    if( m_ss.valid() )
    {
        if( m_glslProgram.valid() )
        {
            //_glslProgram->setName(_name.c_str());
            if( override )
            {
                m_ss->setAttributeAndModes( m_glslProgram.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE );
            }
            else
            {
                m_ss->setAttributeAndModes( m_glslProgram.get(), osg::StateAttribute::ON );
            }
        }
    }
#elif _PERFORMER
    std::cout << "Not implemented for Performer yet!!!" << std::endl;
#endif
}
#ifdef _OSG
//////////////////////////////////////////////////////////////
osg::StateSet* ShaderHelper::GetProgramStateSet()
{
    if( m_ss.valid() )
    {
        return m_ss.get();
    }
    else
    {
        std::cout << "State set is invalid for shader!!" << std::endl;
    }
    return 0;
}
#elif _PERFORMER
#endif
