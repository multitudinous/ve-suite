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
#include "CameraPlacementToolGP.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //
#include <osg/TexGenNode>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolGP::CameraPlacementToolGP()
:
PluginBase(),
mCameraEntity( 0 )
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "CameraPlacementToolUI";

    mEventHandlerMap[ "PROJECTION_UPDATE" ] = this;
    mEventHandlerMap[ "CAMERA_VIEW_UPDATE" ] = this;
    mEventHandlerMap[ "RESOLUTION_UPDATE" ] = this;
    mEventHandlerMap[ "TOGGLE_PROJECTION_UPDATE" ] = this;
    mEventHandlerMap[ "OPACITY_UPDATE" ] = this;
    mEventHandlerMap[ "TOGGLE_CAMERA_UPDATE" ] = this;
    mEventHandlerMap[ "TOGGLE_FRUSTUM_UPDATE" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolGP::~CameraPlacementToolGP()
{
    if( mSceneManager )
    {
        osg::ref_ptr< ves::xplorer::scenegraph::Group > rootNode =
            mSceneManager->GetRootNode();

        if( rootNode.valid() && mCameraEntity.valid() )
        {
            rootNode->removeChild( mCameraEntity.get() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::InitializeNode(
    ves::xplorer::scenegraph::DCS* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    //Initialize the resources for this plugin
    InitializeResources();

    //Initialize the CameraEntity
    mCameraEntity = new cpt::CameraEntity(
        mDCS.get(),
        mEnvironmentHandler->GetHeadsUpDisplay(),
        mResourceManager );

    double cameraPosition[ 3 ] = { 0, -5.0, 0 };
    mCameraEntity->GetDCS()->SetTranslationArray( cameraPosition );

    mSceneManager->GetRootNode()->addChild( mCameraEntity.get() );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::SetCurrentCommand(
    ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }

    if( command->GetCommandName() == "PROJECTION_UPDATE" )
    {
        double projectionData[ 4 ] = { 0, 0, 0, 0 };
        command->GetDataValuePair(
            "projectionFoVZ" )->GetData( projectionData[ 0 ] );
        command->GetDataValuePair(
            "projectionAspectRatio" )->GetData( projectionData[ 1 ] );
        command->GetDataValuePair(
            "projectionNearPlane" )->GetData( projectionData[ 2 ] );
        command->GetDataValuePair(
            "projectionFarPlane" )->GetData( projectionData[ 3 ] );

        mCameraEntity->setProjectionMatrixAsPerspective(
            projectionData[ 0 ], projectionData[ 1 ],
            projectionData[ 2 ], projectionData[ 3 ] );

        mCameraEntity->Update();
    }
    else if( command->GetCommandName() == "CAMERA_VIEW_UPDATE" )
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "viewPerspective" )->GetData( selection );

        bool onOff = ( selection != 0 );
        mCameraEntity->DisplayScreenAlignedQuad( onOff );
    }
    else if( command->GetCommandName() == "RESOLUTION_UPDATE" )
    {
        unsigned int value = 0;
        command->GetDataValuePair( "resolution" )->GetData( value );

        mCameraEntity->SetQuadResolution( value );
    }
    else if( command->GetCommandName() == "TOGGLE_PROJECTION_UPDATE" )
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "toggleProjection" )->GetData( selection );

        bool onOff = ( selection != 0 );
        mCameraEntity->DisplayProjectionEffect( onOff );
    }
    else if( command->GetCommandName() == "OPACITY_UPDATE" )
    {
        double value = 0;
        command->GetDataValuePair( "opacity" )->GetData( value );

        mCameraEntity->SetProjectionEffectOpacity( value );
    }
    else if( command->GetCommandName() == "TOGGLE_CAMERA_UPDATE" )
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "toggleCamera" )->GetData( selection );

        bool onOff = ( selection != 0 );
        mCameraEntity->DisplayCamera( onOff );
    }
    else if( command->GetCommandName() == "TOGGLE_FRUSTUM_UPDATE" )
    {
        unsigned int selection = 0;
        command->GetDataValuePair( "toggleFrustum" )->GetData( selection );

        bool onOff = ( selection != 0 );
        mCameraEntity->DisplayViewFrustum( onOff );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::InitializeResources()
{
    //Create the texture that will be used for the FBO
    //glTexImage2D( target, level, internal format, width, height, border, format, type, *pixels );
    //glTexImage2D( GL_TEXTURE_2D, 0, GL_RGB16F_ARB,  width, height, 0, GL_RGB, GL_FLOAT, NULL );
    osg::ref_ptr< osg::Texture2D > quadTexture = new osg::Texture2D();
    quadTexture->setInternalFormat( GL_RGB16F_ARB );
    quadTexture->setTextureSize( 512, 512 );
    quadTexture->setSourceFormat( GL_RGBA );
    quadTexture->setSourceType( GL_FLOAT );
    quadTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    quadTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    quadTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    quadTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    boost::any anyVal = quadTexture;
    mResourceManager->add( std::string( "CameraViewTexture" ), anyVal );

    //Set up the program for mCameraNode
    {
        std::string cameraVertexSource =
        "varying vec4 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"
            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

            "gl_FrontColor = vec4( 0.7, 0.7, 0.7, 1.0 ); \n"
        "} \n";

        std::string cameraFragmentSource =
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

            "vec4 color = \n"
                "vec4( totalAmbient + totalDiffuse + totalSpecular, 1.0 ); \n"

            "gl_FragColor = color; \n"
        "} \n";

        osg::ref_ptr< osg::Shader > cameraVertexShader = new osg::Shader();
        cameraVertexShader->setType( osg::Shader::VERTEX );
        cameraVertexShader->setShaderSource( cameraVertexSource );

        osg::ref_ptr< osg::Shader > cameraFragmentShader = new osg::Shader();
        cameraFragmentShader->setType( osg::Shader::FRAGMENT );
        cameraFragmentShader->setShaderSource( cameraFragmentSource );

        osg::ref_ptr< osg::Program > cameraProgram = new osg::Program();
        cameraProgram->addShader( cameraVertexShader.get() );
        cameraProgram->addShader( cameraFragmentShader.get() );
        boost::any anyVal = cameraProgram;
        mResourceManager->add( std::string( "CameraProgram" ), anyVal );
    }

    //Set up the program for mFrustumGeode
    {
        std::string frustumVertexSource =
        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_FrontColor = gl_Color; \n"
        "} \n";

        std::string frustumFragmentSource =
        "void main() \n"
        "{ \n"
            "gl_FragColor = gl_Color; \n"
        "} \n";

        osg::ref_ptr< osg::Shader > frustumVertexShader = new osg::Shader();
        frustumVertexShader->setType( osg::Shader::VERTEX );
        frustumVertexShader->setShaderSource( frustumVertexSource );

        osg::ref_ptr< osg::Shader > frustumFragmentShader = new osg::Shader();
        frustumFragmentShader->setType( osg::Shader::FRAGMENT );
        frustumFragmentShader->setShaderSource( frustumFragmentSource );

        osg::ref_ptr< osg::Program > frustumProgram = new osg::Program();
        frustumProgram->addShader( frustumVertexShader.get() );
        frustumProgram->addShader( frustumFragmentShader.get() );
        boost::any anyVal = frustumProgram;
        mResourceManager->add( std::string( "FrustumProgram" ), anyVal );
    }

    //Set up the program for mCameraViewQuadDCS
    {
        std::string hitQuadVertexSource =
        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"
        "} \n";

        std::string hitQuadFragmentSource =
        "void main() \n"
        "{ \n"
            "vec4 color = vec4( 1.0, 0.0, 0.0, 0.3 ); \n"

            "gl_FragColor = color; \n"
        "} \n";

        osg::ref_ptr< osg::Shader > hitQuadVertexShader = new osg::Shader();
        hitQuadVertexShader->setType( osg::Shader::VERTEX );
        hitQuadVertexShader->setShaderSource( hitQuadVertexSource );

        osg::ref_ptr< osg::Shader > hitQuadFragmentShader = new osg::Shader();
        hitQuadFragmentShader->setType( osg::Shader::FRAGMENT );
        hitQuadFragmentShader->setShaderSource( hitQuadFragmentSource );

        osg::ref_ptr< osg::Program > hitQuadProgram = new osg::Program();
        hitQuadProgram->addShader( hitQuadVertexShader.get() );
        hitQuadProgram->addShader( hitQuadFragmentShader.get() );
        boost::any anyVal = hitQuadProgram;
        mResourceManager->add( std::string( "HitQuadProgram" ), anyVal );
    }

    //Set up the program for mCameraViewQuadDCS
    {
        std::string quadVertexSource =
        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "gl_TexCoord[ 1 ].st = gl_MultiTexCoord1.st; \n"
        "} \n";

        std::string quadFragmentSource =
        "uniform sampler2D baseMap; \n"

        "void main() \n"
        "{ \n"
            "vec4 color = texture2D( baseMap, gl_TexCoord[ 1 ].st ); \n"

            "gl_FragColor = color; \n"
        "} \n";

        osg::ref_ptr< osg::Shader > quadVertexShader = new osg::Shader();
        quadVertexShader->setType( osg::Shader::VERTEX );
        quadVertexShader->setShaderSource( quadVertexSource );

        osg::ref_ptr< osg::Shader > quadFragmentShader = new osg::Shader();
        quadFragmentShader->setType( osg::Shader::FRAGMENT );
        quadFragmentShader->setShaderSource( quadFragmentSource );

        osg::ref_ptr< osg::Program > quadProgram = new osg::Program();
        quadProgram->addShader( quadVertexShader.get() );
        quadProgram->addShader( quadFragmentShader.get() );
        boost::any anyVal = quadProgram;
        mResourceManager->add( std::string( "CameraViewProgram" ), anyVal );
    }

    //Set up the camera projection effect
    {
        std::string projectionVertexSource =
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
        "} \n";

        std::string projectionFragmentSource =
        "uniform float alpha; \n"
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
                "vec4( totalAmbient + totalDiffuse + totalSpecular, alpha ); \n"

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
        "} \n";

        osg::ref_ptr< osg::Shader > projectionVertexShader = new osg::Shader();
        projectionVertexShader->setType( osg::Shader::VERTEX );
        projectionVertexShader->setShaderSource( projectionVertexSource );

        osg::ref_ptr< osg::Shader > projectionFragmentShader = new osg::Shader();
        projectionFragmentShader->setType( osg::Shader::FRAGMENT );
        projectionFragmentShader->setShaderSource( projectionFragmentSource );

        osg::ref_ptr< osg::Program > projectionProgram = new osg::Program();
        projectionProgram->addShader( projectionVertexShader.get() );
        projectionProgram->addShader( projectionFragmentShader.get() );
        boost::any anyVal = projectionProgram;
        mResourceManager->add( std::string( "ProjectionProgram" ), anyVal );
    }
}
////////////////////////////////////////////////////////////////////////////////
