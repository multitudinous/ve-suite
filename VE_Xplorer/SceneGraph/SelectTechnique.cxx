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

// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/SelectTechnique.h"
#include "VE_Xplorer/SceneGraph/DCS.h"

// --- OSG Includes --- //
#include <osg/BlendFunc>
#include <osg/CameraNode>
#include <osg/Texture2D>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
SelectTechnique::SelectTechnique( VE_SceneGraph::DCS* dcs )
:
m_dcs( dcs )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
SelectTechnique::~SelectTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SelectTechnique::DefinePasses()
{
    //Implement pass #1
    {
        //Create textures
        const int texWidth = 512;
        const int texHeight = 512;
        osg::ref_ptr< osg::Texture2D > texture = new osg::Texture2D();
        osg::ref_ptr< osg::Texture2D > texture2 = new osg::Texture2D();
        texture->setTextureSize( texWidth, texHeight );
        texture->setFilter( osg::Texture::MIN_FILTER, osg::Texture::LINEAR );
        texture->setFilter( osg::Texture::MAG_FILTER, osg::Texture::LINEAR );
        texture->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP );
        texture->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP );

        //Create a camera that will render the scene to our texture
        osg::ref_ptr< osg::CameraNode > camera = new osg::CameraNode();	

        //Camera setup
        camera->setClearColor( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
        camera->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
        camera->setReferenceFrame( osg::Transform::RELATIVE_RF );
        camera->setViewport( 0, 0, texWidth, texHeight );
        camera->setRenderOrder( osg::CameraNode::NESTED_RENDER );
        camera->setRenderTargetImplementation( osg::CameraNode::FRAME_BUFFER_OBJECT );
        camera->attach( osg::CameraNode::COLOR_BUFFER, GL_RGBA );
        //camera->addChild( m_dcs.get() );
        
        osg::ref_ptr< osg::StateSet > stateset = m_dcs->getStateSet();

        AddPass( stateset.get() );
    }

    {

        char select_vertex_pass2[] =
            "uniform vec2 quadScreenSize; \n"

            "//varying vec4 color; \n"

            "void main() \n"
            "{ \n"
                "float texelIncrement = 1.0 / float( quadScreenSize.x ); \n"
                "vec4 texCoord = gl_MultiTexCoord0; \n"
                "vec3 coord = vec3( texCoord.s, texCoord.t, 1.0 ); \n"
               
                "gl_Position = ftransform(); \n"
                "gl_TexCoord[0] = vec4( coord.s + texelIncrement,       coord.t, texCoord.p, texCoord.q ); \n"
                "gl_TexCoord[1] = vec4( coord.s + texelIncrement * 2.0, coord.t, texCoord.p, texCoord.q ); \n"
                "gl_TexCoord[2] = vec4( coord.s + texelIncrement * 3.0, coord.t, texCoord.p, texCoord.q ); \n"
                "gl_TexCoord[3] = vec4( coord.s + texelIncrement * 4.0, coord.t, texCoord.p, texCoord.q ); \n"
                "gl_TexCoord[4] = vec4( coord.s,                        coord.t, texCoord.p, texCoord.q ); \n"
                "gl_TexCoord[5] = vec4( coord.s - texelIncrement,       coord.t, texCoord.p, texCoord.q ); \n"
                "gl_TexCoord[6] = vec4( coord.s - texelIncrement * 2.0, coord.t, texCoord.p, texCoord.q ); \n"
                "gl_TexCoord[7] = vec4( coord.s - texelIncrement * 3.0, coord.t, texCoord.p, texCoord.q ); \n"
                "//color =        vec4( coord.s - texelIncrement * 4.0, coord.t, texCoord.p, 1.0 ); \n"
            "}";

        char select_fragment_pass2[] =
            "uniform float WT9_0; \n"
            "uniform float WT9_1; \n"
            "uniform float WT9_2; \n"
            "uniform float WT9_3; \n"
            "uniform float WT9_4; \n"

            "uniform sampler2D glowSamp1; \n"

            "//varying vec4 color; \n"

            "void main() \n"
            "{ \n"
                 "float WT9_NORMALIZE = WT9_0 + 2.0 * ( WT9_1 + WT9_2 + WT9_3 + WT9_4 ); \n"
                 "float outCol = texture2D( glowSamp1, gl_TexCoord[0].st ).q * ( WT9_1 / WT9_NORMALIZE ); \n"
                 "outCol +=      texture2D( glowSamp1, gl_TexCoord[1].st ).q * ( WT9_2 / WT9_NORMALIZE ); \n"
                 "outCol +=      texture2D( glowSamp1, gl_TexCoord[2].st ).q * ( WT9_3 / WT9_NORMALIZE ); \n"
                 "outCol +=      texture2D( glowSamp1, gl_TexCoord[3].st ).q * ( WT9_4 / WT9_NORMALIZE ); \n"
                 "outCol +=      texture2D( glowSamp1, gl_TexCoord[4].st ).q * ( WT9_0 / WT9_NORMALIZE ); \n"
                 "outCol +=      texture2D( glowSamp1, gl_TexCoord[5].st ).q * ( WT9_1 / WT9_NORMALIZE ); \n"
                 "outCol +=      texture2D( glowSamp1, gl_TexCoord[6].st ).q * ( WT9_2 / WT9_NORMALIZE ); \n"
                 "outCol +=      texture2D( glowSamp1, gl_TexCoord[7].st ).q * ( WT9_3 / WT9_NORMALIZE ); \n"
                 "//outCol +=      color.q * ( WT9_4 / WT9_NORMALIZE ); \n"
                
                 "gl_FragColor = vec4( outCol, outCol, outCol, outCol ); \n"
            "}";

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;
        osg::ref_ptr< osg::Program > program = new osg::Program;

        osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, select_vertex_pass2 );
        program->addShader( vertex_shader.get() );

        osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, select_fragment_pass2 );
        program->addShader( fragment_shader.get() );

        osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc( GL_ONE, GL_ONE );
        stateset->setAttribute( bf.get() );
        stateset->setMode( GL_BLEND, osg::StateAttribute::ON );

        stateset->setAttributeAndModes( program.get() );

        //stateset->setAttribute(program.get());

        //stateset->setTextureAttributeAndModes( 0,shadow.get(), osg::StateAttribute::ON);
        //stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
        //stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
        //stateset->setTextureMode( 0, GL_TEXTURE_GEN_R, osg::StateAttribute::ON );
        //stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );

        //osg::ref_ptr< osg::Uniform > shadowMap = new osg::Uniform( "shadowMap", 0 );
        //stateset->addUniform( shadowMap.get() );
      
        //AddPass( stateset.get() );
    }

    //Implement pass #3
    {
        char select_vertex_pass3[] =
            "uniform vec2 quadScreenSize; \n"

            "//varying vec4 color; \n"

            "void main() \n"
            "{ \n"
                "float texelIncrement = 1.0 / float( quadScreenSize.y ); \n"
                "vec3 texCoord = gl_MultiTexCoord0.stp; \n"
                "vec3 coord = vec3( texCoord.s, texCoord.t, 1.0 ); \n"
               
                "gl_Position = ftransform(); \n"
                "gl_TexCoord[0] =         vec4( coord.s, coord.t + texelIncrement,       texCoord.p, 1.0 ); \n"
                "gl_TexCoord[1] =         vec4( coord.s, coord.t + texelIncrement * 2.0, texCoord.p, 1.0 ); \n"
                "gl_TexCoord[2] =         vec4( coord.s, coord.t + texelIncrement * 3.0, texCoord.p, 1.0 ); \n"
                "gl_TexCoord[3] =         vec4( coord.s, coord.t + texelIncrement * 4.0, texCoord.p, 1.0 ); \n"
                "gl_TexCoord[4] =         vec4( coord.s, coord.t,                        texCoord.p, 1.0 ); \n"
                "gl_TexCoord[5] =         vec4( coord.s, coord.t - texelIncrement,       texCoord.p, 1.0 ); \n"
                "gl_TexCoord[6] =         vec4( coord.s, coord.t - texelIncrement * 2.0, texCoord.p, 1.0 ); \n"
                "gl_TexCoord[7] =         vec4( coord.s, coord.t - texelIncrement * 3.0, texCoord.p, 1.0 ); \n"
                "//color = vec4( coord.s, coord.t - texelIncrement * 4.0, texCoord.p, 1.0 ); \n"
            "}";

        char select_fragment_pass3[] =
            "uniform float WT9_0; \n"
            "uniform float WT9_1; \n"
            "uniform float WT9_2; \n"
            "uniform float WT9_3; \n"
            "uniform float WT9_4; \n"

            "uniform float glowness; \n"
            "uniform vec4 glowCol; \n"

            "uniform sampler2D glowSamp1; \n"
            "uniform sampler2D glowSamp2; \n"

            "//varying vec4 color; \n"

            "void main() \n"
            "{ \n"
                "float WT9_NORMALIZE = WT9_0 + 2.0 * ( WT9_1 + WT9_2 + WT9_3 + WT9_4 ); \n"
                "float outCol = texture2D( glowSamp2, gl_TexCoord[0].st ).s * ( WT9_1 / WT9_NORMALIZE ); \n"
                "outCol +=      texture2D( glowSamp2, gl_TexCoord[1].st ).s * ( WT9_2 / WT9_NORMALIZE ); \n"
                "outCol +=      texture2D( glowSamp2, gl_TexCoord[2].st ).s * ( WT9_3 / WT9_NORMALIZE ); \n"
                "outCol +=      texture2D( glowSamp2, gl_TexCoord[3].st ).s * ( WT9_4 / WT9_NORMALIZE ); \n"
                "outCol +=      texture2D( glowSamp2, gl_TexCoord[4].st ).s * ( WT9_0 / WT9_NORMALIZE ); \n"
                "outCol +=      texture2D( glowSamp2, gl_TexCoord[5].st ).s * ( WT9_1 / WT9_NORMALIZE ); \n"
                "outCol +=      texture2D( glowSamp2, gl_TexCoord[6].st ).s * ( WT9_2 / WT9_NORMALIZE ); \n"
                "outCol +=      texture2D( glowSamp2, gl_TexCoord[7].st ).s * ( WT9_3 / WT9_NORMALIZE ); \n"
                "//outCol +=      color.w * ( WT9_4 / WT9_NORMALIZE ); \n"
                
                "vec4 glo = ( glowness * outCol ) * glowCol; \n"
                "vec4 orig = texture2D( glowSamp1, gl_TexCoord[4].st ); \n"
                "vec4 final = orig + ( 1.0 - orig.q ) * glo; \n"
                
                "gl_FragColor = final; \n"
            "}";

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;
        osg::ref_ptr< osg::Program > program = new osg::Program;

        osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, select_vertex_pass3 );
        program->addShader( vertex_shader.get() );

        osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, select_fragment_pass3 );
        program->addShader( fragment_shader.get() );

        stateset->setAttributeAndModes( program.get() );

         //AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
