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

    m_imageMap.insert(
        std::make_pair(
            "Decoration",
            xps::ResourceManager::instance()->
                get< osg::Image, osg::ref_ptr >( "./Textures/Decoration.tga" )
            ) );

    m_imageMap.insert(
        std::make_pair(
            "WallMap",
            xps::ResourceManager::instance()->
                get< osg::Image, osg::ref_ptr >( "./Textures/WallMap.tga" )
            ) );

    m_imageMap.insert(
        std::make_pair(
            "Corona",
            xps::ResourceManager::instance()->
                get< osg::Image, osg::ref_ptr >( "./Textures/Corona.tga" )
            ) );

    // Register own map as textures
    typedef std::map< std::string, osg::ref_ptr< osg::Image > >::iterator img_map_iter;
    for( img_map_iter iter = m_imageMap.begin(); iter != m_imageMap.end(); ++iter )
    {
        osg::ref_ptr< osg::Image > tmp_img = iter->second;
        osg::ref_ptr< osg::Texture2D > tmp_tex = new osg::Texture2D( tmp_img.get() );
        boost::any any_val = tmp_tex;
        xps::ResourceManager::instance()->add( iter->first, any_val );
    }

    m_tcm = new osg::TextureCubeMap();
    m_tcm->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setWrap( osg::Texture::WRAP_R, osg::Texture::CLAMP_TO_EDGE );
    m_tcm->setFilter( osg::Texture::MIN_FILTER, osg::Texture::LINEAR );

    m_tcm->setImage(
        osg::TextureCubeMap::POSITIVE_X,
        (xps::ResourceManager::instance()->
            get< osg::Image, osg::ref_ptr >( "./Textures/CubeMap/Right.tga" )).get() );

    m_tcm->setImage(
        osg::TextureCubeMap::NEGATIVE_X,
        (xps::ResourceManager::instance()->
            get< osg::Image, osg::ref_ptr >( "./Textures/CubeMap/Left.tga" )).get() );

    m_tcm->setImage(
        osg::TextureCubeMap::POSITIVE_Y,
        (xps::ResourceManager::instance()->
            get< osg::Image, osg::ref_ptr >( "./Textures/CubeMap/Top.tga" )).get() );

    m_tcm->setImage(
        osg::TextureCubeMap::NEGATIVE_Y,
        (xps::ResourceManager::instance()->
            get< osg::Image, osg::ref_ptr >( "./Textures/CubeMap/Bottom.tga" )).get() );

    m_tcm->setImage(
        osg::TextureCubeMap::POSITIVE_Z,
        (xps::ResourceManager::instance()->
            get< osg::Image, osg::ref_ptr >( "./Textures/CubeMap/Back.tga" )).get() );

    m_tcm->setImage(
        osg::TextureCubeMap::NEGATIVE_Z,
        (xps::ResourceManager::instance()->
            get< osg::Image, osg::ref_ptr >( "./Textures/CubeMap/Front.tga" )).get() );
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
#if 0
    osg::ref_ptr< osg::Program > options_program = new osg::Program();
    options_program->addShader( options_vert_shader.get() );
    options_program->addShader( options_frag_shader.get() );

    any_val = options_program;
    xps::ResourceManager::instance()->add( "OptionsShaderProgram", any_val );
#endif


    // Enumerate the variants
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

    return;
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::SetOptions( osg::ref_ptr< osg::Node > node,
                          bool xray,
                          bool phong,
                          const std::string baseMap,
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

    std::map< std::string, bool > optionsMap;
    optionsMap.insert( std::make_pair( "phong", false ) );
    optionsMap.insert( std::make_pair( "baseMap", false ) );
    optionsMap.insert( std::make_pair( "envMap", false ) );
    optionsMap.insert( std::make_pair( "shadowMap", false ) );

    std::string options_shader_name("OptionsShaderProgram");

    if( phong )
    {
        optionsMap[ "phong" ] = true;
    }

    if( !baseMap.empty() )
    {
        optionsMap[ "baseMap" ] = true;
        options_shader_name+= "-BaseMap";
    }
    else
    {
        options_shader_name+= "-NoBaseMap";
    }

    if( reflectionPercent )
    {
        optionsMap[ "envMap" ] = true;
        options_shader_name+= "-EnvMap";
    }
    else
    {
        options_shader_name+= "-NoEnvMap";
    }

    if( shadow.valid() )
    {
        optionsMap[ "shadowMap" ] = true;
        options_shader_name+= "-ShadowMap";
    }
    else
    {
        options_shader_name+= "-NoShadowMap";
    }

    program = xps::ResourceManager::instance()->
        get< osg::Program, osg::ref_ptr >( options_shader_name );

    stateset->setAttribute( program.get() );

    if( !baseMap.empty() )
    {
        stateset->setTextureAttributeAndModes( 2,
                ( xps::ResourceManager::instance()->
                    get< osg::Texture2D, osg::ref_ptr >( baseMap ) ).get() );
        osg::ref_ptr< osg::Uniform > baseMapUniform = new osg::Uniform( "baseMap", 2 );
        stateset->addUniform( baseMapUniform.get() );
    }

    if( reflectionPercent )
    {
        stateset->setTextureAttributeAndModes( 1, m_tcm.get(), osg::StateAttribute::ON );
        osg::ref_ptr< osg::Uniform > envMap = new osg::Uniform( "envMap", 1 );
        stateset->addUniform( envMap.get() );
    }

    if( shadow.valid() )
    {
        stateset->setTextureAttributeAndModes( 0, shadow.get(), osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_R, osg::StateAttribute::ON );
        stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );

        osg::ref_ptr< osg::Uniform > shadowMap = new osg::Uniform( "shadowMap", 0 );
        stateset->addUniform( shadowMap.get() );
    }

#if 0
    osg::ref_ptr< osg::Uniform > options =
            new osg::Uniform( "options", optionsMap[ "phong" ],
                                         optionsMap[ "baseMap" ],
                                         optionsMap[ "envMap" ],
                                         optionsMap[ "shadowMap" ] );

    stateset->addUniform( options.get() );
#endif

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

    stateset->setTextureAttributeAndModes( 2, new osg::Texture2D( m_imageMap[ "Corona" ].get() ) );

    osg::ref_ptr< osg::Uniform > baseMap = new osg::Uniform( "baseMap", 2 );
    stateset->addUniform( baseMap.get() );

    vertexShader->setShaderSource( lights_vertex );
    fragmentShader->setShaderSource( lights_fragment );
}
////////////////////////////////////////////////////////////////////////////////
