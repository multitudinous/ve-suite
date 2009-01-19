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

    mEventHandlerMap[ "DRUM_ANIMATION_ON_OFF" ] = this;
    mCommandNameToInt[ "DRUM_ANIMATION_ON_OFF" ] =
        DRUM_ANIMATION_ON_OFF;
    mEventHandlerMap[ "CAMERA_GEOMETRY_ON_OFF" ] = this;
    mCommandNameToInt[ "CAMERA_GEOMETRY_ON_OFF" ] =
        CAMERA_GEOMETRY_ON_OFF;
    mEventHandlerMap[ "FRUSTUM_GEOMETRY_ON_OFF" ] = this;
    mCommandNameToInt[ "FRUSTUM_GEOMETRY_ON_OFF" ] =
        FRUSTUM_GEOMETRY_ON_OFF;

    mEventHandlerMap[ "DEPTH_OF_FIELD_EFFECT_ON_OFF" ] = this;
    mCommandNameToInt[ "DEPTH_OF_FIELD_EFFECT_ON_OFF" ] =
        DEPTH_OF_FIELD_EFFECT_ON_OFF;
    mEventHandlerMap[ "PROJECTION_EFFECT_ON_OFF" ] = this;
    mCommandNameToInt[ "PROJECTION_EFFECT_ON_OFF" ] =
        PROJECTION_EFFECT_ON_OFF;
    mEventHandlerMap[ "PROJECTION_EFFECT_OPACITY" ] = this;
    mCommandNameToInt[ "PROJECTION_EFFECT_OPACITY" ] =
        PROJECTION_EFFECT_OPACITY;

    mEventHandlerMap[ "CAMERA_WINDOW_ON_OFF" ] = this;
    mCommandNameToInt[ "CAMERA_WINDOW_ON_OFF" ] =
        CAMERA_WINDOW_ON_OFF;
    mEventHandlerMap[ "CAMERA_WINDOW_RESOLUTION" ] = this;
    mCommandNameToInt[ "CAMERA_WINDOW_RESOLUTION" ] =
        CAMERA_WINDOW_RESOLUTION;

    mEventHandlerMap[ "DEPTH_HELPER_WINDOW_ON_OFF" ] = this;
    mCommandNameToInt[ "DEPTH_HELPER_WINDOW_ON_OFF" ] =
        DEPTH_HELPER_WINDOW_ON_OFF;
    mEventHandlerMap[ "DEPTH_HELPER_WINDOW_RESOLUTION" ] = this;
    mCommandNameToInt[ "DEPTH_HELPER_WINDOW_RESOLUTION" ] =
        DEPTH_HELPER_WINDOW_RESOLUTION;
    
    mEventHandlerMap[ "PROJECTION_UPDATE" ] = this;
    mCommandNameToInt[ "PROJECTION_UPDATE" ] =
        PROJECTION_UPDATE;

    mEventHandlerMap[ "FOCAL_DISTANCE" ] = this;
    mCommandNameToInt[ "FOCAL_DISTANCE" ] =
        FOCAL_DISTANCE;
    mEventHandlerMap[ "FOCAL_RANGE" ] = this;
    mCommandNameToInt[ "FOCAL_RANGE" ] =
        FOCAL_RANGE;
    mEventHandlerMap[ "MAX_CIRCLE_OF_CONFUSION" ] = this;
    mCommandNameToInt[ "MAX_CIRCLE_OF_CONFUSION" ] =
        MAX_CIRCLE_OF_CONFUSION;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolGP::~CameraPlacementToolGP()
{
    if( mSceneManager )
    {
        osg::ref_ptr< osg::Group > rootNode =
            mSceneManager->GetRootNode();

        if( rootNode.valid() && mCameraEntity.valid() )
        {
            rootNode->removeChild( mCameraEntity.get() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::InitializeNode(
    osg::Group* veworldDCS )
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

    const int commandName =
        mCommandNameToInt.find( command->GetCommandName() )->second;

    switch( commandName )
    {
        case DRUM_ANIMATION_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "drumAnimationOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            //mCameraEntity->DrumAnimation( onOff );
        }
        break;

        case CAMERA_GEOMETRY_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "cameraGeometryOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            mCameraEntity->DisplayCamera( onOff );
        }
        break;

        case FRUSTUM_GEOMETRY_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "frustumGeometryOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            mCameraEntity->DisplayViewFrustum( onOff );
        }
        break;

        case DEPTH_OF_FIELD_EFFECT_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "depthOfFieldEffectOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            mCameraEntity->DisplayDepthOfFieldEffect( onOff );
        }
        break;

        case PROJECTION_EFFECT_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "projectionEffectOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            mCameraEntity->DisplayProjectionEffect( onOff );
        }
        break;

        case PROJECTION_EFFECT_OPACITY:
        {
            double value = 0;
            command->GetDataValuePair(
                "projectionEffectOpacity" )->GetData( value );

            mCameraEntity->SetProjectionEffectOpacity( value );
        }
        break;

        case CAMERA_WINDOW_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "cameraWindowOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            mCameraEntity->DisplayCameraViewQuad( onOff );
        }
        break;

        case CAMERA_WINDOW_RESOLUTION:
        {
            unsigned int value = 0;
            command->GetDataValuePair(
                "cameraWindowResolution" )->GetData( value );

            mCameraEntity->SetCameraViewQuadResolution( value );
        }
        break;

        case DEPTH_HELPER_WINDOW_ON_OFF:
        {
            unsigned int selection = 0;
            command->GetDataValuePair(
                "depthHelperWindowOnOff" )->GetData( selection );

            bool onOff = ( selection != 0 );
            mCameraEntity->DisplayDepthHelperQuad( onOff );
        }
        break;

        case DEPTH_HELPER_WINDOW_RESOLUTION:
        {
            unsigned int value = 0;
            command->GetDataValuePair(
                "depthHelperWindowResolution" )->GetData( value );

            mCameraEntity->SetDepthHelperQuadResolution( value );
        }
        break;

        case PROJECTION_UPDATE:
        {
            double projectionData[ 4 ] = { 0, 0, 0, 0 };
            command->GetDataValuePair(
                "projectionFieldOfView" )->GetData( projectionData[ 0 ] );
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
        break;

        case FOCAL_DISTANCE:
        {
            double value = 0;
            command->GetDataValuePair(
                "focalDistance" )->GetData( value );

            mCameraEntity->SetFocalDistance( value );
        }
        break;

        case FOCAL_RANGE:
        {
            double value = 0;
            command->GetDataValuePair(
                "focalRange" )->GetData( value );

            mCameraEntity->SetFocalRange( value );
        }
        break;

        case MAX_CIRCLE_OF_CONFUSION:
        {
            double value = 0;
            command->GetDataValuePair(
                "maxCircleOfConfusion" )->GetData( value );

            mCameraEntity->SetMaxCircleOfConfusion( value );
        }
        break;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolGP::InitializeResources()
{
    /*
    glTexImage2D( target,
                  level,
                  internal format,
                  width,
                  height,
                  border,
                  format,
                  type,
                  *pixels );
    */

    //Create the texture used for the first render target of the camera FBO
    {
        osg::ref_ptr< osg::Texture2D > cameraViewTexture = new osg::Texture2D();
        cameraViewTexture->setInternalFormat( GL_RGB16F_ARB );
        cameraViewTexture->setTextureSize( 1024, 1024 );
        cameraViewTexture->setSourceFormat( GL_RGBA );
        cameraViewTexture->setSourceType( GL_FLOAT );
        cameraViewTexture->setFilter( osg::Texture2D::MIN_FILTER,
                                      osg::Texture2D::LINEAR );
        cameraViewTexture->setFilter( osg::Texture2D::MAG_FILTER,
                                      osg::Texture2D::LINEAR );
        cameraViewTexture->setWrap( osg::Texture2D::WRAP_S,
                                    osg::Texture2D::CLAMP_TO_EDGE );
        cameraViewTexture->setWrap( osg::Texture2D::WRAP_T,
                                    osg::Texture2D::CLAMP_TO_EDGE );
        boost::any anyVal = cameraViewTexture;
        mResourceManager->add( std::string( "CameraViewTexture" ), anyVal );
    }

    //Create the texture used for the second render target of the camera FBO
    {
        osg::ref_ptr< osg::Texture2D > depthTexture = new osg::Texture2D();
        depthTexture->setInternalFormat( GL_RGB16F_ARB );
        depthTexture->setTextureSize( 1024, 1024 );
        depthTexture->setSourceFormat( GL_RGBA );
        depthTexture->setSourceType( GL_FLOAT );
        depthTexture->setFilter( osg::Texture2D::MIN_FILTER,
                                 osg::Texture2D::LINEAR );
        depthTexture->setFilter( osg::Texture2D::MAG_FILTER,
                                 osg::Texture2D::LINEAR );
        depthTexture->setWrap( osg::Texture2D::WRAP_S,
                               osg::Texture2D::CLAMP_TO_EDGE );
        depthTexture->setWrap( osg::Texture2D::WRAP_T,
                               osg::Texture2D::CLAMP_TO_EDGE );
        boost::any anyVal = depthTexture;
        mResourceManager->add( std::string( "DepthTexture" ), anyVal );
    }

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

    //Set up the program for mHitQuadGeode
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

	        "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
        "} \n";

        std::string quadFragmentSource =
        "uniform ivec2 textureDimensions; \n"

        "uniform float maxCoC; \n"

        "uniform sampler2D texture0; \n"
        "uniform sampler2D texture1; \n"

        "void main() \n"
        "{ \n"
            "vec4 colorSum, tapColor; \n"
            "vec2 centerDepthBlur, tapCoord, tapDepthBlur; \n"
	        "float totalContribution, tapContribution; \n"

	        //Poissonian disc distribution
	        "float dx = 1.0 / float( textureDimensions.x ); \n"
	        "float dy = 1.0 / float( textureDimensions.y ); \n"

	        "vec2 filterTaps[ 12 ]; \n"
	        "filterTaps[ 0 ]  = vec2( -0.326212 * dx, -0.405810 * dy ); \n"
	        "filterTaps[ 1 ]  = vec2( -0.840144 * dx, -0.073580 * dy ); \n"
	        "filterTaps[ 2 ]  = vec2( -0.695914 * dx,  0.457137 * dy ); \n"
	        "filterTaps[ 3 ]  = vec2( -0.203345 * dx,  0.620716 * dy ); \n"
	        "filterTaps[ 4 ]  = vec2(  0.962340 * dx, -0.194983 * dy ); \n"
	        "filterTaps[ 5 ]  = vec2(  0.473434 * dx, -0.480026 * dy ); \n"
	        "filterTaps[ 6 ]  = vec2(  0.519456 * dx,  0.767022 * dy ); \n"
	        "filterTaps[ 7 ]  = vec2(  0.185461 * dx, -0.893124 * dy ); \n"
	        "filterTaps[ 8 ]  = vec2(  0.507431 * dx,  0.064425 * dy ); \n"
	        "filterTaps[ 9 ]  = vec2(  0.896420 * dx,  0.412458 * dy ); \n"
	        "filterTaps[ 10 ] = vec2( -0.321940 * dx, -0.932615 * dy ); \n"
	        "filterTaps[ 11 ] = vec2( -0.791559 * dx, -0.597710 * dy ); \n"

	        //Starting with center sample
	        "colorSum = texture2D( texture0, gl_TexCoord[ 0 ].st ); \n"
	        "totalContribution = 1.0; \n"
	        "centerDepthBlur = texture2D( texture1, gl_TexCoord[ 0 ].st ).xy; \n"

	        "float sizeCoC = centerDepthBlur.y * maxCoC; \n"

	        "for( int i = 0; i < 12; ++i ) \n"
            "{ \n"
		        "tapCoord = gl_TexCoord[ 0 ].st + filterTaps[ i ] * sizeCoC; \n"
		        "tapColor = texture2D( texture0, tapCoord ); \n"
		        "tapDepthBlur = texture2D( texture1, tapCoord ).xy; \n"
		        "tapContribution = ( tapDepthBlur.x > centerDepthBlur.x ) ? 1.0 : tapDepthBlur.y; \n"
		        "colorSum += tapColor * tapContribution; \n"
		        "totalContribution += tapContribution; \n"
	        "} \n"

	        "gl_FragColor = colorSum / totalContribution; \n"
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

    {
        std::string DoFRenderBlurVertexSource =
        "void main() \n"
        "{ \n"
	        "gl_Position = ftransform(); \n"

	        "gl_TexCoord[ 0 ] = gl_MultiTexCoord0; \n"
        "} \n";

        std::string DoFRenderBlurFragmentSource =
        "uniform sampler2D texture1; \n"

        "void main() \n"
        "{ \n"
	        "vec4 blurDepth = texture2D( texture1, gl_TexCoord[ 0 ].st ); \n"
	        "gl_FragColor = vec4( blurDepth.x * 0.03, blurDepth.y, 0.0, 1.0 ); \n"
        "} \n";

        osg::ref_ptr< osg::Shader > renderBlurVertexShader = new osg::Shader();
        renderBlurVertexShader->setType( osg::Shader::VERTEX );
        renderBlurVertexShader->setShaderSource( DoFRenderBlurVertexSource );

        osg::ref_ptr< osg::Shader > renderBlurFragmentShader = new osg::Shader();
        renderBlurFragmentShader->setType( osg::Shader::FRAGMENT );
        renderBlurFragmentShader->setShaderSource( DoFRenderBlurFragmentSource );

        osg::ref_ptr< osg::Program > renderBlurProgram = new osg::Program();
        renderBlurProgram->addShader( renderBlurVertexShader.get() );
        renderBlurProgram->addShader( renderBlurFragmentShader.get() );
        boost::any anyVal = renderBlurProgram;
        mResourceManager->add( std::string( "RenderBlurProgram" ), anyVal );
    }

    //Set up the camera projection effect
    {
        std::string projectionVertexSource =
        "varying float fDepth; \n"

        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "varying vec4 eyePos; \n"

        "void main() \n"
        "{ \n"
            "gl_Position = ftransform(); \n"

            "lightPos = gl_LightSource[ 0 ].position.xyz; \n"
            "normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
            "eyePos = gl_ModelViewMatrix * gl_Vertex; \n"

            "fDepth = -eyePos.z; \n"

            "gl_FrontColor = gl_Color; \n"

            "gl_TexCoord[ 0 ].s = dot( eyePos, gl_EyePlaneS[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].t = dot( eyePos, gl_EyePlaneT[ 0 ] ); \n"
            "gl_TexCoord[ 0 ].q = dot( eyePos, gl_EyePlaneQ[ 0 ] ); \n"
        "} \n";

        std::string projectionFragmentSource =
        "uniform float alpha; \n"
        "uniform float nearPlane; \n"
        "uniform float farPlane; \n"

        "uniform float focalDistance; \n"
        "uniform float focalRange; \n"

        "varying float fDepth; \n"

        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "varying vec4 eyePos; \n"

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
            "vec4 color0 = \n"
                "vec4( totalAmbient + totalDiffuse + totalSpecular, alpha ); \n"

            //If in frustum
            "if( projectionUV.s >= 0.0 && projectionUV.s <= 1.0 && \n"
                "projectionUV.t >= 0.0 && projectionUV.t <= 1.0 && \n"
                "gl_TexCoord[ 0 ].q >= nearPlane && \n"
                "gl_TexCoord[ 0 ].q <= farPlane ) \n"
            "{ \n"
                "color0.a = 1.0; \n"
            "} \n"

            "float tempFocalRange = 2.0 / focalRange; \n"
            "float tempSat =  abs( fDepth - focalDistance ) * tempFocalRange; \n"
            //"float blur = saturate( tempSat ); \n"
            "float blur = clamp( tempSat, 0.0, 1.0 ); \n"
            "vec4 color1 = vec4( fDepth, blur, 0.0, 1.0 ); \n"

            "gl_FragData[ 0 ] = color0; \n"
            "gl_FragData[ 1 ] = color1; \n"
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
