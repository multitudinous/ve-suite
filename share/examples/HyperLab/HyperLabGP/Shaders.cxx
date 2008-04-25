/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- My Includes --- //
#include "VertFrag.h"
#include "Shaders.h"

#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/StateSet>
#include <osg/Texture2D>
#include <osg/TextureCubeMap>

#include <osgDB/ReadFile>

// --- C/C++ Libraries --- //

using namespace hyperlab;
namespace xps = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Shaders::Shaders()
{
    ReadTextures();
    InitializeShaders();
}
////////////////////////////////////////////////////////////////////////////////
Shaders::~Shaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::ReadTextures()
{
    osg::ref_ptr< osg::Texture2D > decorationTexture = new osg::Texture2D();
    decorationTexture->setFilter(
        osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    decorationTexture->setFilter(
        osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    decorationTexture->setWrap(
        osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    decorationTexture->setWrap(
        osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    decorationTexture->setImage(
        osgDB::readImageFile( "Textures/Decoration.tga" ) );

    boost::any decorationVal = decorationTexture;
    ves::xplorer::scenegraph::ResourceManager::instance()->add(
        std::string( "Decoration" ), decorationVal );

    osg::ref_ptr< osg::Texture2D > wallMapTexture = new osg::Texture2D();
    wallMapTexture->setFilter(
        osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    wallMapTexture->setFilter(
        osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    wallMapTexture->setWrap(
        osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    wallMapTexture->setWrap(
        osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    wallMapTexture->setImage(
        osgDB::readImageFile( "Textures/WallMap.tga" ) );

    boost::any wallMapVal = wallMapTexture;
    ves::xplorer::scenegraph::ResourceManager::instance()->add(
        std::string( "WallMap" ), wallMapVal );

    osg::ref_ptr< osg::Texture2D > coronaTexture = new osg::Texture2D();
    coronaTexture->setFilter(
        osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    coronaTexture->setFilter(
        osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    coronaTexture->setWrap(
        osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    coronaTexture->setWrap(
        osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    coronaTexture->setImage(
        osgDB::readImageFile( "Textures/Corona.tga" ) );

    boost::any coronaVal = coronaTexture;
    ves::xplorer::scenegraph::ResourceManager::instance()->add(
        std::string( "Corona" ), coronaVal );

    osg::ref_ptr< osg::TextureCubeMap > textureCubeMap =
        new osg::TextureCubeMap();
    textureCubeMap->setWrap(
        osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    textureCubeMap->setWrap(
        osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    textureCubeMap->setWrap(
        osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    textureCubeMap->setFilter(
        osg::Texture::MIN_FILTER, osg::Texture::LINEAR );

    textureCubeMap->setImage( osg::TextureCubeMap::POSITIVE_X,
        osgDB::readImageFile( "Textures/CubeMap/Right.tga" ) );
    textureCubeMap->setImage( osg::TextureCubeMap::NEGATIVE_X,
        osgDB::readImageFile( "Textures/CubeMap/Left.tga" ) );
    textureCubeMap->setImage( osg::TextureCubeMap::POSITIVE_Y,
        osgDB::readImageFile( "Textures/CubeMap/Top.tga" ) );
    textureCubeMap->setImage( osg::TextureCubeMap::NEGATIVE_Y,
        osgDB::readImageFile( "Textures/CubeMap/Bottom.tga" ) );
    textureCubeMap->setImage( osg::TextureCubeMap::POSITIVE_Z,
        osgDB::readImageFile( "Textures/CubeMap/Back.tga" ) );
    textureCubeMap->setImage( osg::TextureCubeMap::NEGATIVE_Z,
        osgDB::readImageFile( "Textures/CubeMap/Front.tga" ) );

    boost::any cubeMapVal = textureCubeMap;
    ves::xplorer::scenegraph::ResourceManager::instance()->add(
        std::string( "CubeMap" ), cubeMapVal );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::InitializeShaders()
{
    osg::ref_ptr< osg::Shader > xray_vert_shader = new osg::Shader();
    xray_vert_shader->setType( osg::Shader::VERTEX );
    xray_vert_shader->setShaderSource( xray_vertex );

    osg::ref_ptr< osg::Shader > xray_frag_shader = new osg::Shader();
    xray_frag_shader->setShaderSource( xray_fragment );
    xray_frag_shader->setType( osg::Shader::FRAGMENT );

    osg::ref_ptr< osg::Program > xray_program = new osg::Program();
    xray_program->addShader( xray_vert_shader.get() );
    xray_program->addShader( xray_frag_shader.get() );

    boost::any any_val = xray_program;
    xps::ResourceManager::instance()->add( "XRayShaderProgram", any_val );

    osg::ref_ptr< osg::Shader > options_vert_shader = new osg::Shader();
    options_vert_shader->setType( osg::Shader::VERTEX );
    options_vert_shader->setShaderSource( options_vertex );

    osg::ref_ptr< osg::Shader > options_frag_shader = new osg::Shader();
    options_frag_shader->setType( osg::Shader::FRAGMENT );
    options_frag_shader->setShaderSource( options_fragment );

    //Enumerate the variants
    osg::ref_ptr< osg::Shader > options_basemap_frag_shader = new osg::Shader();
    options_basemap_frag_shader->setType( osg::Shader::FRAGMENT );
    options_basemap_frag_shader->setShaderSource( options_base_map_fragment );

    osg::ref_ptr< osg::Shader > options_no_basemap_frag_shader = new osg::Shader();
    options_no_basemap_frag_shader->setType( osg::Shader::FRAGMENT );
    options_no_basemap_frag_shader->setShaderSource( options_base_map_fragment_off );

    osg::ref_ptr< osg::Shader > options_envmap_frag_shader = new osg::Shader();
    options_envmap_frag_shader->setType( osg::Shader::FRAGMENT );
    options_envmap_frag_shader->setShaderSource( options_env_map_fragment );

    osg::ref_ptr< osg::Shader > options_no_envmap_frag_shader = new osg::Shader();
    options_no_envmap_frag_shader->setType( osg::Shader::FRAGMENT );
    options_no_envmap_frag_shader->setShaderSource( options_env_map_fragment_off );

    osg::ref_ptr< osg::Shader > options_shadowmap_frag_shader = new osg::Shader();
    options_shadowmap_frag_shader->setType( osg::Shader::FRAGMENT );
    options_shadowmap_frag_shader->setShaderSource( options_shadow_map_fragment );

    osg::ref_ptr< osg::Shader > options_no_shadowmap_frag_shader = new osg::Shader();
    options_no_shadowmap_frag_shader->setType( osg::Shader::FRAGMENT );
    options_no_shadowmap_frag_shader->setShaderSource( options_shadow_map_fragment_off );


    // Make and save the combos
    for( unsigned int i = 0; i <= 1; ++i )
    {
        for( unsigned int j = 0; j <= 1; ++j )
        {
            for( unsigned int k = 0; k <= 1; ++k )
            {
                osg::ref_ptr< osg::Program > new_opt_prog = new osg::Program();
                new_opt_prog->addShader( options_vert_shader.get() );
                new_opt_prog->addShader( options_frag_shader.get() );
                std::string shadName("OptionsShaderProgram");
                if( i == 0 ) // Choose basemap shader
                {
                    new_opt_prog->addShader( options_basemap_frag_shader.get() );
                    shadName += "-BaseMap";
                }
                else
                {
                    new_opt_prog->addShader( options_no_basemap_frag_shader.get() );
                    shadName += "-NoBaseMap";
                }
                if( j == 0 ) // Choose envmap shader
                {
                    new_opt_prog->addShader( options_envmap_frag_shader.get() );
                    shadName += "-EnvMap";
                }
                else
                {
                    new_opt_prog->addShader( options_no_envmap_frag_shader.get() );
                    shadName += "-NoEnvMap";
                }
                if( k == 0 ) // Choose envmap shader
                {
                    new_opt_prog->addShader( options_shadowmap_frag_shader.get() );
                    shadName += "-ShadowMap";
                }
                else
                {
                    new_opt_prog->addShader( options_no_shadowmap_frag_shader.get() );
                    shadName += "-NoShadowMap";
                }
                any_val = new_opt_prog;
                xps::ResourceManager::instance()->add( shadName, any_val );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::SetOptions( osg::ref_ptr< osg::Node > node,
                          bool xray,
                          bool phong,
                          const std::string& baseMap,
                          float* reflectionPercent,
                          osg::ref_ptr< osg::Texture2D > shadow )
{
    osg::ref_ptr< osg::StateSet > stateset = node->getOrCreateStateSet();
    osg::ref_ptr< osg::Program > program;

    if( xray )
    {
        program = xps::ResourceManager::instance()->
            get< osg::Program, osg::ref_ptr >( "XRayShaderProgram" );
        stateset->setAttribute( program.get() );
        stateset->setRenderBinDetails( 10, "DepthSortedBin" );
        stateset->setMode( GL_BLEND, osg::StateAttribute::ON );

        return;
    }

    std::string options_shader_name( "OptionsShaderProgram" );

    if( !baseMap.empty() )
    {
        stateset->setTextureAttributeAndModes( 2,
            ( xps::ResourceManager::instance()->get
            < osg::Texture2D, osg::ref_ptr >( baseMap ) ).get(),
            osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > baseMapUniform =
            new osg::Uniform( "baseMap", 2 );
        stateset->addUniform( baseMapUniform.get() );

        options_shader_name += "-BaseMap";
    }
    else
    {
        options_shader_name += "-NoBaseMap";
    }

    if( reflectionPercent )
    {
        stateset->setTextureAttributeAndModes( 1,
            ( xps::ResourceManager::instance()->get
            < osg::TextureCubeMap, osg::ref_ptr >( "CubeMap" ) ).get(),
            osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > envMap =
            new osg::Uniform( "envMap", 1 );
        stateset->addUniform( envMap.get() );

        options_shader_name += "-EnvMap";
    }
    else
    {
        options_shader_name += "-NoEnvMap";
    }

    if( shadow.valid() )
    {
        stateset->setTextureAttributeAndModes(
            0, shadow.get(), osg::StateAttribute::ON );
        stateset->setTextureMode(
            0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
        stateset->setTextureMode(
            0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
        stateset->setTextureMode(
            0, GL_TEXTURE_GEN_R, osg::StateAttribute::ON );
        stateset->setTextureMode(
            0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > shadowMap =
            new osg::Uniform( "shadowMap", 0 );
        stateset->addUniform( shadowMap.get() );

        options_shader_name += "-ShadowMap";
    }
    else
    {
        options_shader_name += "-NoShadowMap";
    }

    program = xps::ResourceManager::instance()->
        get< osg::Program, osg::ref_ptr >( options_shader_name );
    stateset->setAttribute( program.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Lights( osg::ref_ptr< osg::Node > node )
{
    osg::ref_ptr< osg::StateSet > stateset = node->getOrCreateStateSet();
    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    stateset->setAttribute( program.get() );

    stateset->setTextureAttributeAndModes( 2,
        ( xps::ResourceManager::instance()->get
        < osg::Texture2D, osg::ref_ptr >( "Corona" ) ).get(),
        osg::StateAttribute::ON );

    osg::ref_ptr< osg::Uniform > baseMap = new osg::Uniform( "baseMap", 2 );
    stateset->addUniform( baseMap.get() );

    vertexShader->setShaderSource( lights_vertex );
    fragmentShader->setShaderSource( lights_fragment );
}
////////////////////////////////////////////////////////////////////////////////
