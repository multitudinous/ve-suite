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
#include "CameraPlacementToolShaders.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolShaders::CameraPlacementToolShaders()
{
    CreateShaderSources();
    CreateShaderPrograms();
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolShaders::~CameraPlacementToolShaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolShaders::CreateShaderSources()
{
    mProjectionVertexSource = std::string(
    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
        "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
        "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

        "gl_FrontColor = gl_Color; \n"

        "gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] ); \n"
        "gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] ); \n"
        "gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] ); \n"
    "} \n" );

    mProjectionFragmentSource = std::string(
    "uniform float nearPlane; \n"
    "uniform float farPlane; \n"

    "varying vec4 eyePos; \n"
    "varying vec3 lightPos; \n"
    "varying vec3 normal; \n"

    "void main() \n"
    "{ \n"
        "vec3 N = normalize( normal ); \n"
        "vec3 L = normalize( lightPos ); \n"
        "float NDotL = max( dot( N, L ), 0.0 ); \n"

        "vec3 V = normalize( eyePos.xyz ); \n"
        "vec3 R = reflect( V, N ); \n"
        "float RDotL = max( dot( R, L ), 0.0 ); \n"

        "vec3 totalAmbient = gl_LightSource[ 0 ].ambient.rgb * \n"
                            "gl_Color.rgb; \n"
        "vec3 totalDiffuse = gl_LightSource[ 0 ].diffuse.rgb * \n"
                            "gl_Color.rgb * NDotL; \n"
        "vec3 totalSpecular = gl_LightSource[ 0 ].specular.rgb * \n"
                             "gl_Color.rgb * pow( RDotL, 15.0 ); \n"

        "vec2 projectionUV = gl_TexCoord[ 0 ].st / gl_TexCoord[ 0 ].q; \n"
        "vec4 color = \n"
            "vec4( totalAmbient + totalDiffuse + totalSpecular, 0.2 ); \n"

        //If in frustum
        "if( projectionUV.s >= 0.0 && \n"
            "projectionUV.t >= 0.0 && \n"
            "projectionUV.s <= 1.0 && \n"
            "projectionUV.t <= 1.0 && \n"
            "gl_TexCoord[ 0 ].q >= nearPlane && \n"
            "gl_TexCoord[ 0 ].q <= farPlane ) \n"
        "{ \n"
            "color.a = 1.0; \n"
        "} \n"

        "gl_FragColor = color; \n"
    "} \n" );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolShaders::CreateShaderPrograms()
{
    osg::ref_ptr< osg::Shader > projectionVertexShader = new osg::Shader();
    projectionVertexShader->setType( osg::Shader::VERTEX );
    projectionVertexShader->setShaderSource( mProjectionVertexSource );

    osg::ref_ptr< osg::Shader > projectionFragmentShader = new osg::Shader();
    projectionFragmentShader->setType( osg::Shader::FRAGMENT );
    projectionFragmentShader->setShaderSource( mProjectionFragmentSource );

    osg::ref_ptr< osg::Program > projectionProgram = new osg::Program();
    projectionProgram->addShader( projectionVertexShader.get() );
    projectionProgram->addShader( projectionFragmentShader.get() );
    boost::any anyVal = projectionProgram;
    ves::xplorer::scenegraph::ResourceManager::instance()->add(
        std::string( "ProjectionProgram" ), anyVal );
}
////////////////////////////////////////////////////////////////////////////////
