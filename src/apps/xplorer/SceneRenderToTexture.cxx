/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

// --- VE-Suite Includes --- //
#include "SceneRenderToTexture.h"
#include "MSMRTCallbacks.h"

#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/environment/cfdDisplaySettings.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/rtt/Processor.h>
#include <ves/xplorer/scenegraph/rtt/UnitCameraAttachmentBypass.h>
#include <ves/xplorer/scenegraph/rtt/UnitInOut.h>
#include <ves/xplorer/scenegraph/rtt/UnitInResampleOut.h>
#include <ves/xplorer/scenegraph/rtt/UnitOut.h>

#include <ves/xplorer/Debug.h>

#include <ves/open/xml/Command.h>

// ---  VR Juggler Includes --- //
#include <vrj/Draw/OpenGL/Window.h>
#include <vrj/Draw/OpenGL/DrawManager.h>
#include <vrj/Draw/OpenGL/ContextData.h>

#include <vrj/Display/SurfaceViewport.h>
#include <vrj/Display/Frustum.h>
#include <vrj/Display/Projection.h>

#include <gmtl/gmtl.h>
#include <gmtl/Misc/MatrixConvert.h>

#include <boost/concept_check.hpp>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Camera>
#include <osg/Depth>
#include <osg/FrameBufferObject>
#include <osg/Light>
#include <osg/LightSource>
#include <osg/LightModel>

#include <osgDB/WriteFile>
#include <osgDB/ReaderWriter>
#include <osgDB/ReadFile>

#include <osgUtil/SceneView>
#include <osgUtil/UpdateVisitor>

#include <osgwTools/ScreenCapture.h>

// --- BackdropFX Includes --- //
#include <backdropFX/Version.h>
#include <backdropFX/Manager.h>
#include <backdropFX/SkyDome.h>
#include <backdropFX/DepthPartition.h>
#include <backdropFX/DepthPeelUtils.h>
#include <backdropFX/RenderingEffects.h>
#include <backdropFX/EffectLibraryUtils.h>
#include <backdropFX/LocationData.h>
#include <backdropFX/ShaderModule.h>
#include <backdropFX/ShaderModuleUtils.h>
#include <backdropFX/ShaderModuleVisitor.h>

// --- STL Includes --- //
#include <iostream>

//#define VES_SRTT_DEBUG
/*#ifndef _DARWIN
#define VES_USE_MSMRT_CALLBACK
#define VES_USE_KEEP_FBOS_BOUND_CALLBACK
#define VES_USE_MULTISAMPLING
#endif*/

using namespace ves::xplorer;

////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::SceneRenderToTexture( bool const& enableRTT )
    :
    m_enableRTT( enableRTT ),
    m_rootGroup( new osg::Group() ),
    m_1dxVP( NULL ),
    m_1dxFP( NULL ),
    m_1dyVP( NULL ),
    m_1dyFP( NULL ),
    m_finalShader( NULL ),
    m_light0( new osg::Light() ),
    m_lightSource0( new osg::LightSource() ),
    m_lightModel0( new osg::LightModel() )
{
    /// When m_enableRTT is true we will use our old RTT and post processing 
    /// pipeline. When it is false we will use bdfx.
    if( m_enableRTT )
    {
        osg::ref_ptr< osgDB::ReaderWriter::Options > vertexOptions =
            new osgDB::ReaderWriter::Options( "vertex" );
        osg::ref_ptr< osgDB::ReaderWriter::Options > fragmentOptions =
            new osgDB::ReaderWriter::Options( "fragment" );

        try
        {
            m_1dxVP = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dx_vp.glsl", vertexOptions.get() );
            m_1dxFP = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dx_fp.glsl", fragmentOptions.get() );
        }
        catch( ... )
        {
            std::cerr << "Could not load shader files!" << std::endl;
        }

        try
        {
            m_1dyVP = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dy_vp.glsl", vertexOptions.get() );
            m_1dyFP = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dy_fp.glsl", fragmentOptions.get() );
        }
        catch( ... )
        {
            std::cerr << "Could not load shader files!" << std::endl;
        }

        try
        {
            m_finalShader =
                osgDB::readShaderFile( "glsl/final_fp.glsl", fragmentOptions.get() );
        }
        catch( ... )
        {
            std::cerr << "Could not load shader files!" << std::endl;
        }

        //
        InitRootGroup();
    }
    else
    {
        //The order of initialization matters substantially for dbfx

        //backdropFX::SkyDome& skyDome =
        //    backdropFX::Manager::instance()->getSkyDome();
        //skyDome.setSunScale( 2.0 );
        //skyDome.setMoonScale( 2.0 );

        //Add root group to backdropFX::Manager
        backdropFX::Manager::instance()->setSceneData( m_rootGroup.get() );

        // Disable depth partitioning.
        backdropFX::Manager::instance()->getDepthPartition().setNumPartitions( 1 );
        
        backdropFX::Manager::instance()->rebuild( 0 );//backdropFX::Manager::depthPeel );

        //
        InitRootGroup();
    }
}
////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::~SceneRenderToTexture()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitRootGroup()
{
    scenegraph::SceneManager::instance()->SetRootNode( m_rootGroup.get() );

    //If we are in rtt mode, set the main shader
    if( m_enableRTT )
    {
        //Setup the MRT shader to make glow work correctly
        osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
        std::string fragmentSource =
        "uniform bool textureZeroIsBound; \n"
        "uniform vec3 glowColor; \n"
        "uniform sampler2D tex0; \n"

        "void main() \n"
        "{ \n"
        "    gl_FragData[ 0 ] = gl_Color; \n"
        "    gl_FragData[ 1 ] = vec4( glowColor, gl_FragData[ 0 ].a ); \n"
        "\n"
        "    if( textureZeroIsBound ) \n"
        "    { \n"
                //GL_MODULATE
        "        gl_FragData[ 0 ] *= texture2D( tex0, gl_TexCoord[ 0 ].st ); \n"
        "    } \n"
        "} \n";

        fragmentShader->setType( osg::Shader::FRAGMENT );
        fragmentShader->setShaderSource( fragmentSource );
        fragmentShader->setName( "Root Group Fragment Shader" );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragmentShader.get() );
        program->setName( "Root Group Program" );

        osg::ref_ptr< osg::StateSet > stateset =
            m_rootGroup->getOrCreateStateSet();
        stateset->setAttributeAndModes(
            program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        //Used to control whether a file that has been loaded has a texture
        //attached. This should be used by any shader to ensure that textures
        //are handled appropriately.
        stateset->addUniform(
            new osg::Uniform( "textureZeroIsBound", false ) );
        //Default glow color for any children that don't explicitly set it
        stateset->addUniform(
            new osg::Uniform( "glowColor", osg::Vec3( 0.0, 0.0, 0.0 ) ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::SetupDefaultLighting()
{
    m_light0->setLightNum( 0 );
    m_light0->setAmbient( osg::Vec4d( 0.36862, 0.36842, 0.36842, 1.0 ) );
    m_light0->setDiffuse( osg::Vec4d( 0.88627, 0.88500, 0.88500, 1.0 ) );
    m_light0->setSpecular( osg::Vec4d( 0.49019, 0.48872, 0.48872, 1.0 ) );
    //We are in openGL space
    m_light0->setPosition( osg::Vec4d( 0.0, 10000.0, 10000.0, 0.0 ) );
    //m_light0->setDirection( osg::Vec3d( 0.0, 1.0, -1.0 ) );

    m_lightSource0->setLight( m_light0.get() );
    m_lightSource0->setLocalStateSetModes( osg::StateAttribute::ON );
    //See the opengl docs on the difference between ABSOLUTE and RELATIVE
    m_lightSource0->setReferenceFrame( osg::LightSource::RELATIVE_RF );

    m_lightModel0->setAmbientIntensity( osg::Vec4( 0.1, 0.1, 0.1, 1.0 ) );
    //Get correct specular lighting across pipes
    //See http://www.ds.arch.tue.nl/General/Staff/Joran/osg/osg_specular_problem.htm
    m_lightModel0->setLocalViewer( true );

    osg::ref_ptr< osg::StateSet > lightStateSet =
        m_rootGroup->getOrCreateStateSet();
    lightStateSet->setAssociatedModes( m_light0.get(), osg::StateAttribute::ON );
    lightStateSet->setMode( GL_LIGHTING, osg::StateAttribute::ON );
    lightStateSet->setAttributeAndModes(
        m_lightModel0.get(), osg::StateAttribute::ON );
    m_rootGroup->addChild( m_lightSource0.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitScene( osg::Camera* const svCamera )
{
    //Setup our lighting now
    SetupDefaultLighting();

    /*
    if( !scenegraph::SceneManager::instance()->IsRTTOn() )
    {
        m_updateList.push_back( svCamera );
        m_captureTools[ svCamera ] = 
            new osgwTools::ScreenCapture( "test_image", ".png", true );
        return;
    }
    */

    //Get window and viewport information
    vrj::opengl::DrawManager* glDrawManager =
        vrj::opengl::DrawManager::instance();
    vrj::opengl::UserData* userData = glDrawManager->currentUserData();
    vrj::opengl::WindowPtr glWindow = userData->getGlWindow();
    vrj::DisplayPtr display = glWindow->getDisplay();

    //Get state info about the screen
    int contextOriginX, contextOriginY, contextWidth, contextHeight;
    display->getOriginAndSize(
        contextOriginX, contextOriginY, contextWidth, contextHeight );

    std::cout << "|\tContext Screen Dimensions: "
              << " ( " << contextWidth
              << ", "  << contextHeight << " )"
              << std::endl;

    unsigned int const numViewports =
        static_cast< unsigned int >( display->getNumViewports() );

    std::cout << "|\tNumber of Viewports: " << numViewports << std::endl;
    std::cout << "|\t" << std::endl;

    //This assumes that all viewports for this context are the same size
    vrj::ViewportPtr viewport = display->getViewport( 0 );

    //Get state info about the viewport
    float vp_ox, vp_oy, vp_sx, vp_sy;
    viewport->getOriginAndSize( vp_ox, vp_oy, vp_sx, vp_sy );

    //
    const std::pair< int, int > viewportDimensions(
        static_cast< int >( vp_sx * contextWidth ),
        static_cast< int >( vp_sy * contextHeight ) );

    //
    InitRTTCamera( svCamera, viewportDimensions );

    if( m_enableRTT )
    {
        //
        svCamera->addChild( CreateClearColorQuad( numViewports ) );

        //
        svCamera->addChild( m_rootGroup.get() );

        //
        (*m_postProcessCamera) = CreatePostProcessCamera();

        //
        (*m_postProcessCamera)->addChild(
            CreatePostProcessPipeline( svCamera, viewportDimensions ) );

        //Add the post process pipeline to the sv camera
        svCamera->addChild( (*m_postProcessCamera).get() );
    }
    else
    {
        backdropFX::ShaderModuleVisitor smv;
        smv.setAttachMain( false ); // Use bdfx-main
        smv.setAttachTransform( false ); // Use bdfx-transform
        smv.setSupportSunLighting( false ); // Use shaders that support Sun lighting.
        
        backdropFX::convertFFPToShaderModules( m_rootGroup.get(), &smv );
        
        //
        backdropFX::Manager::instance()->setTextureWidthHeight(
            viewportDimensions.first, viewportDimensions.second );

        //Add managed root to each SceneView
        svCamera->addChild(
            backdropFX::Manager::instance()->getManagedRoot() );

        backdropFX::RebuildShaderModules rsm;
        backdropFX::Manager::instance()->getManagedRoot()->accept( rsm );
    }

    //Make sure that existing scene graph objects are
    //allocated with thread safe ref/unref
    svCamera->setThreadSafeRefUnref( true );

    //Update the scene graph so that it has enough GL object buffer
    //memory for the graphics contexts that will be using it
    svCamera->resizeGLObjectBuffers(
        osg::DisplaySettings::instance()->getMaxNumberOfGraphicsContexts() );

    m_updateList.push_back( svCamera );
    m_captureTools[ svCamera ] =
        new osgwTools::ScreenCapture( "test_image", ".png", true );

    (*m_camerasConfigured) = true;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitRTTCamera(
    osg::Camera* const rttCamera,
    std::pair< int, int > const& viewportDimensions )
{
    //Clear color quad will clear color and depth buffers for us
    rttCamera->setClearMask( 0 );
    rttCamera->setClearColor( osg::Vec4( 0.0, 1.0, 0.0, 0.0 ) );

    if( m_enableRTT )
    {
#ifdef VES_USE_MSMRT_CALLBACK
        //Post-draw callback on root camera handles resolving
        //multisampling for the MRT case
        MSMRTCallback* msmrt = new MSMRTCallback( rttCamera );
        rttCamera->setPostDrawCallback( msmrt );
#endif

        rttCamera->setRenderTargetImplementation(
            osg::Camera::FRAME_BUFFER_OBJECT, osg::Camera::FRAME_BUFFER_OBJECT );
        rttCamera->setComputeNearFarMode(
            osg::CullSettings::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
        //Viewport cannot be outside the buffer (texture)
        rttCamera->setViewport(
            0, 0, viewportDimensions.first, viewportDimensions.second );

        //Set up the color map
        //Most GPUs prefer the BGRA format
        //http://www.opengl.org/wiki/Common_Mistakes
        osg::ref_ptr< osg::Texture2D > colorMap = CreateViewportTexture(
            GL_RGBA8, GL_BGRA, GL_UNSIGNED_BYTE,
            osg::Texture2D::LINEAR, osg::Texture2D::CLAMP_TO_EDGE,
            viewportDimensions );

        //Set up the glow map
        osg::ref_ptr< osg::Texture2D > glowMap = CreateViewportTexture(
            GL_RGBA8, GL_BGRA, GL_UNSIGNED_BYTE,
            osg::Texture2D::LINEAR, osg::Texture2D::CLAMP_TO_EDGE,
            viewportDimensions );

        //Attach a texture and use it as the render target
        //If you set one buffer to multisample, they all get set to multisample
        //see RenderStage.cpp
        int maxSamples( 0 );
#ifdef VES_USE_MULTISAMPLING
        glGetIntegerv( GL_MAX_SAMPLES_EXT, &maxSamples );
        if( maxSamples > 4 )
        {
            maxSamples = 4;
        }
#endif

        rttCamera->attach(
            osg::Camera::COLOR_BUFFER0, colorMap.get(),
            0, 0, false, maxSamples, maxSamples );
        rttCamera->attach(
            osg::Camera::COLOR_BUFFER1, glowMap.get(),
            0, 0, false, maxSamples, maxSamples );
        //Use interleaved depth/stencil renderbuffer
        rttCamera->attach(
            osg::Camera::PACKED_DEPTH_STENCIL_BUFFER, GL_DEPTH_STENCIL_EXT );

        //Set up the depth buffer
        //osg::ref_ptr< osg::Texture2D > depthMap = CreateViewportTexture(
            //GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE,
            //osg::Texture2D::NEAREST, osg::Texture2D::CLAMP_TO_EDGE,
            //viewportDimensions );

        //Set up interleaved depth/stencil buffer
        //osg::ref_ptr< osg::Texture2D > depthStencilMap = CreateViewportTexture(
            //GL_DEPTH24_STENCIL8_EXT, GL_DEPTH_STENCIL_EXT, GL_UNSIGNED_INT_24_8_EXT,
            //osg::Texture2D::NEAREST, osg::Texture2D::CLAMP_TO_EDGE,
            //viewportDimensions );
    }
    else
    {
        ///With depth partitioning enabled this should be set to: 
        ///DO_NOT_COMPUTE_NEAR_FAR
        rttCamera->setComputeNearFarMode(
            osg::CullSettings::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* SceneRenderToTexture::CreatePostProcessCamera()
{
    osg::Camera* postProcessCamera = new osg::Camera();
    postProcessCamera->setName( "Post Process Camera" );
    postProcessCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
    postProcessCamera->setRenderOrder( osg::Camera::POST_RENDER, 0 );
    postProcessCamera->setRenderTargetImplementation(
        osg::Camera::FRAME_BUFFER, osg::Camera::FRAME_BUFFER );
    //RTT quad will clear color and depth buffers for us
    postProcessCamera->setClearMask( 0x00000000 );
    postProcessCamera->setComputeNearFarMode(
        osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
    postProcessCamera->setCullingActive( false );
    postProcessCamera->setThreadSafeRefUnref( true );
    postProcessCamera->setViewMatrix( osg::Matrix::identity() );
    postProcessCamera->setProjectionMatrix( osg::Matrix::identity() );

    return postProcessCamera;
}
////////////////////////////////////////////////////////////////////////////////
scenegraph::rtt::Processor* SceneRenderToTexture::CreatePostProcessPipeline(
    osg::Camera* rttCamera,
    std::pair< int, int > const& viewportDimensions )
{
    //This is the code for the post-processing pipeline
    scenegraph::rtt::Processor* postProcessPipeline =
        new scenegraph::rtt::Processor();
    postProcessPipeline->SetCamera( rttCamera );

    //COLOR_BUFFER0 bypass
    osg::ref_ptr< scenegraph::rtt::UnitCameraAttachmentBypass > colorBuffer0 =
        new scenegraph::rtt::UnitCameraAttachmentBypass();
    {
        colorBuffer0->setName( "ColorBuffer0Bypass" );
        colorBuffer0->SetBufferComponent( osg::Camera::COLOR_BUFFER0 );
        colorBuffer0->SetInputTextureIndexForViewportReference( 0 );
    }
    postProcessPipeline->addChild( colorBuffer0.get() );
    colorBuffer0->Update();

    //COLOR_BUFFER1 bypass
    osg::ref_ptr< scenegraph::rtt::UnitCameraAttachmentBypass > colorBuffer1 =
         new scenegraph::rtt::UnitCameraAttachmentBypass();
    {
        colorBuffer1->setName( "ColorBuffer1Bypass" );
        colorBuffer1->SetBufferComponent( osg::Camera::COLOR_BUFFER1 );
        colorBuffer1->SetInputTextureIndexForViewportReference( 0 );
    }
    postProcessPipeline->addChild( colorBuffer1.get() );
    colorBuffer1->Update();

    //Downsample by 1/2 original size
    osg::Vec2 quadScreenSize(
        viewportDimensions.first, viewportDimensions.second );
    osg::ref_ptr< scenegraph::rtt::UnitInResampleOut > glowDownSample =
        new scenegraph::rtt::UnitInResampleOut();
    {
        float downsample = 0.5;
        quadScreenSize *= downsample;

        glowDownSample->setName( "GlowDownSample" );
        glowDownSample->SetFactorX( downsample );
        glowDownSample->SetFactorY( downsample );
        //glowDownSample->SetInputTextureIndexForViewportReference( 0 );
    }
    colorBuffer1->addChild( glowDownSample.get() );
    glowDownSample->Update();

    //Perform horizontal 1D gauss convolution
    osg::ref_ptr< scenegraph::rtt::UnitInOut > blurX =
        new scenegraph::rtt::UnitInOut();
    {
        osg::ref_ptr< osg::Program > gaussX = new osg::Program();

        //Setup horizontal blur shaders
        gaussX->addShader( m_1dxVP.get() );
        gaussX->addShader( m_1dxFP.get() );
        gaussX->setName( "BlurHorizontalShader" );

        osg::ref_ptr< osg::Uniform > quadScreenSizeUniform =
            new osg::Uniform( "quadScreenSize", quadScreenSize );
        osg::ref_ptr< osg::Uniform > WT9_0Uniform =
            new osg::Uniform( "WT9_0", static_cast< float >( 0.5 ) );
        osg::ref_ptr< osg::Uniform > WT9_1Uniform =
            new osg::Uniform( "WT9_1", static_cast< float >( 0.4 ) );
        osg::ref_ptr< osg::Uniform > WT9_2Uniform =
            new osg::Uniform( "WT9_2", static_cast< float >( 0.3 ) );
        osg::ref_ptr< osg::Uniform > WT9_3Uniform =
            new osg::Uniform( "WT9_3", static_cast< float >( 0.2 ) );
        osg::ref_ptr< osg::Uniform > WT9_4Uniform =
            new osg::Uniform( "WT9_4", static_cast< float >( 0.1 ) );

        osg::ref_ptr< osg::StateSet > stateSet = blurX->getOrCreateStateSet();
        stateSet->setAttribute(
            gaussX.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateSet->addUniform( quadScreenSizeUniform.get() );
        stateSet->addUniform( WT9_0Uniform.get() );
        stateSet->addUniform( WT9_1Uniform.get() );
        stateSet->addUniform( WT9_2Uniform.get() );
        stateSet->addUniform( WT9_3Uniform.get() );
        stateSet->addUniform( WT9_4Uniform.get() );

        blurX->setName( "BlurHorizontal" );
        blurX->SetInputToUniform( glowDownSample.get(), "glowMap", true );
        blurX->SetInputTextureIndexForViewportReference( 0 );
    }
    blurX->Update();

    //Perform vertical 1D gauss convolution
    osg::ref_ptr< scenegraph::rtt::UnitInOut > blurY =
        new scenegraph::rtt::UnitInOut();
    {
        osg::ref_ptr< osg::Program > gaussY = new osg::Program();

        //Setup vertical blur shaders
        gaussY->addShader( m_1dyVP.get() );
        gaussY->addShader( m_1dyFP.get() );
        gaussY->setName( "BlurVerticalShader" );

        osg::ref_ptr< osg::Uniform > quadScreenSizeUniform =
            new osg::Uniform( "quadScreenSize", quadScreenSize );
        osg::ref_ptr< osg::Uniform > WT9_0Uniform =
            new osg::Uniform( "WT9_0", static_cast< float >( 0.5 ) );
        osg::ref_ptr< osg::Uniform > WT9_1Uniform =
            new osg::Uniform( "WT9_1", static_cast< float >( 0.4 ) );
        osg::ref_ptr< osg::Uniform > WT9_2Uniform =
            new osg::Uniform( "WT9_2", static_cast< float >( 0.3 ) );
        osg::ref_ptr< osg::Uniform > WT9_3Uniform =
            new osg::Uniform( "WT9_3", static_cast< float >( 0.2 ) );
        osg::ref_ptr< osg::Uniform > WT9_4Uniform =
            new osg::Uniform( "WT9_4", static_cast< float >( 0.1 ) );

        osg::ref_ptr< osg::StateSet > stateSet = blurY->getOrCreateStateSet();
        stateSet->setAttribute(
            gaussY.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateSet->addUniform( quadScreenSizeUniform.get() );
        stateSet->addUniform( WT9_0Uniform.get() );
        stateSet->addUniform( WT9_1Uniform.get() );
        stateSet->addUniform( WT9_2Uniform.get() );
        stateSet->addUniform( WT9_3Uniform.get() );
        stateSet->addUniform( WT9_4Uniform.get() );

        blurY->setName( "BlurVertical" );
        blurY->SetInputToUniform( blurX.get(), "glowMap", true );
        blurY->SetInputTextureIndexForViewportReference( 0 );
    }
    blurY->Update();

    //Perform final color operations and blends
    osg::ref_ptr< scenegraph::rtt::UnitInOut > final =
        new scenegraph::rtt::UnitInOut();
    {
        osg::ref_ptr< osg::Program > finalShader = new osg::Program();

        //Setup vertical blur shaders
        finalShader->addShader( m_finalShader.get() );
        finalShader->setName( "FinalShader" );

        osg::ref_ptr< osg::Uniform > glowStrengthUniform =
            new osg::Uniform( "glowStrength", static_cast< float >( 8.0 ) );

        osg::ref_ptr< osg::StateSet > stateSet = final->getOrCreateStateSet();
        stateSet->setAttribute(
            finalShader.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateSet->addUniform( glowStrengthUniform.get() );

        final->setName( "Final" );
        final->SetInputToUniform( colorBuffer0.get(), "baseMap", false );
        final->SetInputToUniform(
            colorBuffer1.get(), "stencilMap", false );
        final->SetInputToUniform( blurY.get(), "glowMap", true );
        final->SetInputTextureIndexForViewportReference( 0 );
    }
    final->Update();

    //Render to the Frame Buffer
    postProcessPipeline->addChild( CreateRTTQuad(
        static_cast< osg::Texture2D* >( final->GetOutputTexture() ) ) );

    return postProcessPipeline;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* SceneRenderToTexture::CreateViewportTexture(
    GLenum internalFormat,
    GLenum sourceFormat,
    GLenum sourceType,
    osg::Texture2D::FilterMode filterMode,
    osg::Texture2D::WrapMode wrapMode,
    std::pair< int, int > const& viewportDimensions )
{
    //GL_RGBA8/GL_UNSIGNED_INT: 8 bits per channel,  32 bits total
    //GL_RGBA16F_ARB/GL_FLOAT: 16 bits per channel,  64 bits total
    //GL_RGBA32F_ARB/GL_FLOAT: 32 bits per channel, 128 bits total
    osg::Texture2D* tempTexture = new osg::Texture2D();
    tempTexture->setInternalFormat( internalFormat );
    tempTexture->setSourceFormat( sourceFormat );
    tempTexture->setSourceType( sourceType );
    tempTexture->setFilter( osg::Texture2D::MIN_FILTER, filterMode );
    tempTexture->setFilter( osg::Texture2D::MAG_FILTER, filterMode );
    tempTexture->setWrap( osg::Texture2D::WRAP_S, wrapMode );
    tempTexture->setWrap( osg::Texture2D::WRAP_T, wrapMode );
    tempTexture->setTextureSize(
        viewportDimensions.first, viewportDimensions.second );

    return tempTexture;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* SceneRenderToTexture::CreateClearColorQuad(
    unsigned int const& numViewports )
{
    boost::ignore_unused_variable_warning( numViewports );

    //Get the vertex coordinates for the quad
    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );
    (*quadVertices)[ 0 ].set( -1.0, -1.0, 1.0 );
    (*quadVertices)[ 1 ].set(  1.0, -1.0, 1.0 );
    (*quadVertices)[ 2 ].set(  1.0,  1.0, 1.0 );
    (*quadVertices)[ 3 ].set( -1.0,  1.0, 1.0 );

    //Create the quad geometry
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();
    quadGeometry->setVertexArray( quadVertices.get() );
    quadGeometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, quadVertices->size() ) );
    quadGeometry->setUseDisplayList( true );
    quadGeometry->setColorBinding( osg::Geometry::BIND_OFF );

    //Create geode for quad
    osg::Geode* quadGeode = new osg::Geode();
    quadGeode->setName( "Clear Color Quad" );
    quadGeode->addDrawable( quadGeometry.get() );
    quadGeode->setCullingActive( false );

#ifdef VES_USE_KEEP_FBOS_BOUND_CALLBACK
    //Do not unbind the FBOs after the BlitFramebuffer call
    quadGeode->setCullCallback( new KeepFBOsBoundCallback( numViewports ) );
#endif

    //
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    std::string vertexSource =
    "void main() \n"
    "{ \n"
        //Ignore MVP transformation as vertices are already in Normalized Device Coord
        "gl_Position = gl_Vertex; \n"
    "} \n";

    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );
    vertexShader->setName( "VS Quad Vertex Shader" );

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    std::string fragmentSource =
    "uniform vec4 clearColor; \n"

    "void main() \n"
    "{ \n"
        "gl_FragData[ 0 ] = clearColor; \n"
        "gl_FragData[ 1 ] = vec4( 0.0, 0.0, 0.0, 0.0 ); \n"
    "} \n";

    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );
    fragmentShader->setName( "VS Quad Fragment Shader" );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    program->setName( "VS Quad Program" );

    //Don't write to the depth buffer
    osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    depth->setWriteMask( true );
    depth->setFunction( osg::Depth::ALWAYS );

    //Set stateset for quad
    osg::ref_ptr< osg::StateSet > stateset = quadGeode->getOrCreateStateSet();
    //Render first
    stateset->setRenderBinDetails( -1, "RenderBin" );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    stateset->setMode(
        GL_DEPTH_TEST,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes(
        depth.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->addUniform(
        &scenegraph::SceneManager::instance()->GetClearColorUniform() );

    return quadGeode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* SceneRenderToTexture::CreateRTTQuad( osg::Texture2D* texture )
{
    //Create geode for quad
    osg::Geode* rttQuad = new osg::Geode();
    rttQuad->setName( "RTT Quad" );
    rttQuad->setCullingActive( false );

    //Get the vertex coordinates for the quad
    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );
    (*quadVertices)[ 0 ].set( -1.0, -1.0, 1.0 );
    (*quadVertices)[ 1 ].set(  1.0, -1.0, 1.0 );
    (*quadVertices)[ 2 ].set(  1.0,  1.0, 1.0 );
    (*quadVertices)[ 3 ].set( -1.0,  1.0, 1.0 );

    //Get the texture coordinates for the quad
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0.0, 0.0 );
    (*quadTexCoords)[ 1 ].set( 1.0, 0.0 );
    (*quadTexCoords)[ 2 ].set( 1.0, 1.0 );
    (*quadTexCoords)[ 3 ].set( 0.0, 1.0 );

    //Create the quad geometry
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();
    quadGeometry->setVertexArray( quadVertices.get() );
    quadGeometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, quadVertices->size() ) );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    quadGeometry->setUseDisplayList( true );
    quadGeometry->setColorBinding( osg::Geometry::BIND_OFF );
    rttQuad->addDrawable( quadGeometry.get() );

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    std::string vertexSource =
    "void main() \n"
    "{ \n"
        //Ignore MVP transformation as vertices are already in Normalized Device Coord.
        "gl_Position = gl_Vertex; \n"
        "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
    "} \n";

    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );
    vertexShader->setName( "VS Quad Vertex Shader" );

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    std::string fragmentSource =
    "uniform sampler2D baseMap; \n"

    "void main() \n"
    "{ \n"
        "vec3 base = texture2D( baseMap, gl_TexCoord[ 0 ].st ).rgb; \n"
#ifdef VES_SRTT_DEBUG
        "gl_FragData[ 0 ] = vec4( 1.0, 1.0, 0.0, 1.0 ); \n"
#else
        "gl_FragData[ 0 ] = vec4( base, 1.0 ); \n"
#endif
    "} \n";

    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );
    fragmentShader->setName( "VS Quad Fragment Shader" );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    program->setName( "VS Quad Program" );

    //Don't write to the depth buffer
    //osg::ref_ptr< osg::Depth > depth = new osg::Depth();
    //depth->setWriteMask( false );

    //Set stateset for quad
    osg::ref_ptr< osg::StateSet > stateset = rttQuad->getOrCreateStateSet();
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    stateset->setMode(
        GL_DEPTH_TEST,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    //stateset->setAttributeAndModes(
        //depth.get(),
        //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );
    stateset->setTextureAttributeAndModes(
        0, texture,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

#ifdef VES_SRTT_DEBUG
    rttQuad->setNodeMask( 0 );
#endif

    return rttQuad;
}
////////////////////////////////////////////////////////////////////////////////
osg::Light* SceneRenderToTexture::GetLight0() const
{
    return m_light0.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* SceneRenderToTexture::GetRootGroup() const
{
    return m_rootGroup.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* SceneRenderToTexture::GetPostProcessCamera()
{
    return (*m_postProcessCamera).get();
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::WriteImageFileForWeb(
    osg::Group* root, osgUtil::SceneView* sv, std::string& filename )
{
    /*
    while( runWebImageSaveThread )
    {
        vpr::System::msleep( 500 );  // half-second delay
        if( readyToWriteWebImage )
        {
            readyToWriteWebImage = false;
            writingWebImageNow = true;
            //let's try saving the image with Corona
            corona::Image* frameCap = corona::CreateImage(
                webImageWidth, webImageHeight,
                corona::PF_R8G8B8, (void*)webImagePixelArray );
            frameCap = corona::FlipImage( frameCap, corona::CA_X );
            if( !corona::SaveImage( "../../public_html/PowerPlant/VE/dump.png",
                                    corona::FF_PNG, frameCap ) )
            {
                std::cout << "error saving image!" << std::endl;
            }
            else
            {
                std::cout << "Image saved successfully.!" << std::endl;
            }
            delete frameCap;
            delete [] webImagePixelArray;                 //delete our array
            std::cout << "All done!" << std::endl;
            writingWebImageNow = false;
        }
    }
    */

    ///Setup all the images for rendering
    osg::ref_ptr< osg::Image > shot = new osg::Image();
    std::vector< osg::ref_ptr< osg::Image > > imageList;
    // get the image ratio:
    int w = 0; int  h = 0;
    EnvironmentHandler::instance()->GetDesktopSize( w, h );
    int largeWidth =  w * 2; 
    int largeHeight = h * 2 ;
    shot->allocateImage( largeWidth, largeHeight, 1, GL_RGB, GL_UNSIGNED_BYTE );

    ///Now lets create the scene
    osg::ref_ptr< osg::Node > subgraph = new osg::Group( *root );
    std::vector< osg::ref_ptr< osg::Camera > > rttCameraList;

    ///create the screen shot root
    osg::ref_ptr< osg::Group > screenShotRoot = new osg::Group;

    ///create the list of RTT's
    std::vector< osg::ref_ptr< osg::Texture2D > >rttList;

    //osg::ref_ptr<osgUtil::SceneView> sv;
    //sv = ( *sceneViewer );  // Get context specific scene viewer
    osg::ref_ptr<osg::Camera> oldcamera = sv->getCamera();
    //Copy the settings from sceneView-camera to
    //get exactly the view the user sees at the moment:
    //Get the current frustum from the current sceneView-camera
    double frustum[6] = {0, 0, 0, 0, 0, 0};
    oldcamera->getProjectionMatrixAsFrustum(
        frustum[ 0 ], frustum[ 1 ], frustum[ 2 ], frustum[ 3 ], frustum[ 4 ], frustum[ 5 ] );
    //Create 4 cameras whose frustums tile the original camera frustum
    double tileFrustum[6] = { 0, 0, 0, 0, 0, 0 };
    //z values don't change
    tileFrustum[ 4 ] = frustum[ 4 ];
    tileFrustum[ 5 ] = frustum[ 5 ];

    std::vector< osg::ref_ptr< osg::Texture2D > > textures;
    for( size_t i = 0; i < 4; ++i )
    {
        //Set up the RTT's (Render-To-Textures)
        //The output textures here are 2x as big as the desired tile
        //This gives us more information to fight aliasing by "super-sampling"
        //at the desired resolution 
        rttList.push_back( new osg::Texture2D( ) );
        rttList.back()->setTextureSize( w * 2, h * 2 );
        rttList.back()->setInternalFormat( GL_RGB );
        rttList.back()->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
        rttList.back()->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
        rttList.back()->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
        rttList.back()->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );

        //Setup the cameras
        rttCameraList.push_back( new osg::Camera );
        rttCameraList.back()->setClearColor( oldcamera->getClearColor() );
        rttCameraList.back()->setClearMask( oldcamera->getClearMask() );
        rttCameraList.back()->setColorMask( oldcamera->getColorMask() );
        rttCameraList.back()->setTransformOrder( oldcamera->getTransformOrder() );
        rttCameraList.back()->setViewMatrix( oldcamera->getViewMatrix() );

        // set view
        rttCameraList.back()->setReferenceFrame( osg::Transform::ABSOLUTE_RF );

        // set the camera to render before after the main camera.
        rttCameraList.back()->setRenderOrder( osg::Camera::PRE_RENDER );

        // tell the camera to use OpenGL frame buffer object where supported.
        rttCameraList.back()->setRenderTargetImplementation(
            osg::Camera::FRAME_BUFFER_OBJECT );
        // add subgraph to render
        rttCameraList.back()->addChild( subgraph.get() );

        // set viewport
        rttCameraList.back()->setViewport( 0, 0, w*2, h*2 );

        ///Attach the camera to the image
        rttCameraList.back()->attach(
            osg::Camera::COLOR_BUFFER, rttList.back().get() );
        screenShotRoot->addChild( rttCameraList.back().get() );
    }

    std::vector< osg::ref_ptr<osg::Camera> >::iterator activeCamera;
    ///
    {
        //setup ll
        activeCamera = rttCameraList.begin();
        //left
        tileFrustum[ 0 ] = frustum[ 0 ];
        //right
        tileFrustum[ 1 ] = frustum[ 0 ] + ( frustum[ 1 ] - frustum[ 0 ] ) * .5;
        //bottom
        tileFrustum[ 2 ] = frustum[ 2 ];
        //top
        tileFrustum[ 3 ] = frustum[ 3 ] + ( frustum[ 2 ] - frustum[ 3 ] ) * .5;
        (*activeCamera)->setProjectionMatrixAsFrustum(
            tileFrustum[ 0 ], tileFrustum[ 1 ], tileFrustum[ 2 ],
            tileFrustum[ 3 ], tileFrustum[ 4 ], tileFrustum[ 5 ] );
    }
    ///
    {
        //setup lr
        activeCamera = rttCameraList.begin() + 1;
        //left
        tileFrustum[ 0 ] = frustum[ 0 ] + .5 * ( frustum[ 1 ] - frustum[ 0 ] );
        //right
        tileFrustum[ 1 ] = frustum[ 1 ];
        //bottom
        tileFrustum[ 2 ] = frustum[ 2 ];
        //top
        tileFrustum[ 3 ] = frustum[ 3 ] + ( frustum[ 2 ] - frustum[ 3 ] ) * .5;
        (*activeCamera)->setProjectionMatrixAsFrustum(
            tileFrustum[ 0 ], tileFrustum[ 1 ], tileFrustum[ 2 ],
            tileFrustum[ 3 ], tileFrustum[ 4 ], tileFrustum[ 5 ] );
    }
    ///
    {
        //setup ur
        activeCamera = rttCameraList.begin() + 2;
        //left
        tileFrustum[ 0 ] = frustum[ 0 ] + .5 * ( frustum[ 1 ] - frustum[ 0 ] );
        //right
        tileFrustum[ 1 ] = frustum[ 1 ];
        //bottom
        tileFrustum[ 2 ] = frustum[ 3 ] + ( frustum[ 2 ] - frustum[ 3 ] ) * .5;
        //top
        tileFrustum[ 3 ] = frustum[ 3 ];
        ( *activeCamera )->setProjectionMatrixAsFrustum(
            tileFrustum[ 0 ], tileFrustum[ 1 ], tileFrustum[ 2 ],
            tileFrustum[ 3 ], tileFrustum[ 4 ], tileFrustum[ 5 ] );
    }
    ///
    {
        //setup ul
        activeCamera = rttCameraList.begin() + 3;
        //left
        tileFrustum[ 0 ] = frustum[ 0 ];
        //right
        tileFrustum[ 1 ] = frustum[ 0 ] + .5 * ( frustum[ 1 ] - frustum[ 0 ] );
        //bottom
        tileFrustum[ 2 ] = frustum[ 3 ] + ( frustum[ 2 ] - frustum[ 3 ] ) * .5;
        //top
        tileFrustum[ 3 ] = frustum[ 3 ];
        ( *activeCamera )->setProjectionMatrixAsFrustum(
            tileFrustum[ 0 ], tileFrustum[ 1 ], tileFrustum[ 2 ],
            tileFrustum[ 3 ], tileFrustum[ 4 ], tileFrustum[ 5 ] );
    }
    //Add the screen shot as a pre-render node of the main graph
    root->addChild( screenShotRoot.get() );

    //Render to produce the tiles via RTT
    sv->update();
    sv->cull();
    sv->draw();
    /*
    if(rttList.at(0)->getImage())osgDB::writeImageFile( *(rttList.at(0)->getImage()), "texture1.jpg" );
    if(rttList.at(1)->getImage())osgDB::writeImageFile( *(rttList.at(1)->getImage()), "texture2.jpg" );
    if(rttList.at(2)->getImage())osgDB::writeImageFile( *(rttList.at(2)->getImage()), "texture3.jpg" );
    if(rttList.at(3)->getImage())osgDB::writeImageFile( *(rttList.at(3)->getImage()), "texture4.jpg" );
    */

    //Set up the full screen quads to apply the RTT's to
    std::vector< osg::ref_ptr< osg::Geode > > fullScreenQuads;

    //Take 9 samples per pixel to super-sample the image
    //Can easily change this to a guassian or some other convolution filter
    //This takes an average of 8 neighboring samples in the higher-res image
    //and writes them to our output
    osg::ref_ptr< osg::Program > ssaaProgram = new osg::Program();
    ssaaProgram->setName( "9-Samples Per Pixel" );
    char fragmentShaderSource[] =
    "uniform sampler2D baseTexture; \n"
    "uniform vec2 dimensions;\n"
    "\n"
    "void main(void) \n"
    "{\n"
    "  vec2 sample = 1.0/(dimensions*2.0);\n"
    "  vec4 sum = texture2D(baseTexture,gl_TexCoord[ 0 ].xy );\n"
    "  //if( ( gl_TexCoord[ 0 ].x > 0.0 ) &&\n"
    "  //    ( gl_TexCoord[ 0 ].x < 1.0 ) &&\n"
    "  //    ( gl_TexCoord[ 0 ].y > 0.0 ) &&\n"
    "  //    ( gl_TexCoord[ 0 ].y < 1.0 ) )\n"
    "    {\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy + sample);\n"
    "        vec2 diff = vec2(-sample.x, sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy + diff);\n"
    
    "        diff = vec2(-sample.x, -sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy + diff);\n"
    
    "        diff = vec2(sample.x, -sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy + diff);\n"
    
    "        diff = vec2(sample.x, 0.0);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy + diff);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy - diff);\n"
    
    "        diff = vec2(0.0, sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy + diff);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[ 0 ].xy - diff);\n"
    
    "        sum /= 9.0;\n"
    "    }\n"
    "    gl_FragColor = normalize(sum);\n"
    "}\n";
    ssaaProgram->addShader( new osg::Shader( osg::Shader::FRAGMENT, fragmentShaderSource ) );

    //Is this overkill???
    //Probably can use only 1 quad and hook them up to the 4 cameras
    //but this is the brute force way...
    for(unsigned int i = 0; i < 4; ++i )
    {
        fullScreenQuads.push_back( new osg::Geode );
        fullScreenQuads.back()->getOrCreateStateSet()->
        setTextureAttributeAndModes( static_cast<int>(i),
                                    rttList.at(i).get(),
                                    osg::StateAttribute::ON );
        fullScreenQuads.back()->getStateSet()->addUniform(
            new osg::Uniform ( "baseTexture", static_cast< int >( i ) ) );
        fullScreenQuads.back()->getStateSet()->addUniform(
            new osg::Uniform ( "dimensions", osg::Vec2( w * 2, h * 2 ) ) );
        fullScreenQuads.back()->getStateSet()->setAttribute( ssaaProgram.get() );
        //float scoord = 1.0 / (float)( w * 2 );
        //float tcoord = 1.0 / (float)( h * 2 );
        fullScreenQuads.back()->addDrawable( osg::createTexturedQuadGeometry(
            osg::Vec3( -1.0f, -1.0f, -1.0f ), osg::Vec3( 2.0f, 0.0f, 0.0f ),
            osg::Vec3( 0.0f, 2.0f, 0.0f ), 0.0f, 0.0f, 1.0f, 1.0f ) );
    }

    //create the FBO's for ssaa for each tile
    //The output image here is the actual size that we want
    std::vector< osg::ref_ptr< osg::Image > > ssImageList;
    std::vector< osg::ref_ptr<osg::Camera> > fullScreenQuadCameraList;
    for( unsigned int i = 0; i < 4; ++i )
    {
        //the four sampled images
        ssImageList.push_back( new osg::Image() );
        ssImageList.back()->allocateImage( w, h, 1, GL_RGB, GL_UNSIGNED_BYTE );
        
        // reset subgraph to render to the quad
        fullScreenQuadCameraList.push_back( new osg::Camera );
        fullScreenQuadCameraList.back()->setClearColor( osg::Vec4( 0, 0, 0, 0) );
        fullScreenQuadCameraList.back()->setClearMask( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
        fullScreenQuadCameraList.back()->setReferenceFrame( osg::Transform::ABSOLUTE_RF_INHERIT_VIEWPOINT );
        fullScreenQuadCameraList.back()->setRenderOrder( osg::Camera::PRE_RENDER );
        fullScreenQuadCameraList.back()->setRenderTargetImplementation(
            osg::Camera::FRAME_BUFFER_OBJECT );

        fullScreenQuadCameraList.back()->setViewport( 0, 0, w, h );
        fullScreenQuadCameraList.back()->setViewMatrix( osg::Matrix::identity() );
        fullScreenQuadCameraList.back()->setProjectionMatrix( osg::Matrix::identity() );
        
        ///Attach the camera to the output image
        fullScreenQuadCameraList.back()->attach( osg::Camera::COLOR_BUFFER,
                                                ssImageList.back().get() );

        fullScreenQuadCameraList.back()->addChild( fullScreenQuads.at( i ).get() );
        //swap out the cameras 
        screenShotRoot->replaceChild( rttCameraList.at( i ).get(), fullScreenQuadCameraList.back().get() );
    }

    //render the scene again to create the ssaa image
    sv->update();
    sv->cull();
    sv->draw();
    /*
    osgDB::writeImageFile( *(ssImageList.at( 0 )), "ssImage1.jpg" );
    osgDB::writeImageFile( *(ssImageList.at( 1 )), "ssImage2.jpg" );
    osgDB::writeImageFile( *(ssImageList.at( 2 )), "ssImage3.jpg" );
    osgDB::writeImageFile( *(ssImageList.at( 3 )), "ssImage4.jpg" );
    */

    //remove the screen shot from the graph
    root->removeChild( screenShotRoot.get() );

    ///Now put the images together
    std::vector< osg::ref_ptr< osg::Image > >::iterator activeImage;
    //setup ll
    activeImage = ssImageList.begin();
    shot->copySubImage( 0, 0, 0, ( *activeImage ).get() );
    //setup lr
    activeImage = ssImageList.begin() + 1;
    shot->copySubImage( w, 0, 0, ( *activeImage ).get() );
    //setup ur
    activeImage = ssImageList.begin() + 2;
    shot->copySubImage( w, h, 0, ( *activeImage ).get() );
    //setup ul
    activeImage = ssImageList.begin() + 3;
    shot->copySubImage( 0, h, 0, ( *activeImage ).get() );
    //This would work, too:
    osgDB::writeImageFile( *( shot.get() ), filename );
}
////////////////////////////////////////////////////////////////////////////////
bool SceneRenderToTexture::CameraConfigured()
{
    return (*m_camerasConfigured);
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitializeRTT()
{
    (*m_camerasConfigured) = false;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::Update(
    osg::NodeVisitor* updateVisitor, ves::open::xml::CommandPtr tempCommand )
{
    std::string tempCommandName;

    if( tempCommand )
    {
        tempCommandName = tempCommand->GetCommandName();
    }

    for( std::vector< osg::Camera* >::iterator iter = m_updateList.begin();
        iter != m_updateList.end(); ++iter )
    {
        (*iter)->accept( *updateVisitor );

        //This code came from osgViewer::Viewer::setSceneData
        //The resize stuff is what is critical not sure how important it is
        if( !tempCommandName.compare( "veNetwork Update" ) )
        {
            //Make sure that existing scene graph objects are 
            //allocated with thread safe ref/unref
            (*iter)->setThreadSafeRefUnref(true);

            //Update the scene graph so that it has enough GL object buffer
            //memory for the graphics contexts that will be using it.
            (*iter)->resizeGLObjectBuffers( 
                osg::DisplaySettings::instance()->
                getMaxNumberOfGraphicsContexts() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::WriteLowResImageFile(
    osg::Group* root, osgUtil::SceneView* sv, std::string& filename )
{
    ///Setup all the images for rendering
    osg::ref_ptr< osg::Image > shot = new osg::Image();
    //std::vector< osg::ref_ptr< osg::Image > > imageList;
    // get the image ratio:
    std::pair< int, int > screenDims =
        EnvironmentHandler::instance()->GetDisplaySettings()->GetScreenResolution();
    int w = 0; int  h = 0; int m = 1;
    w = screenDims.first;
    h = screenDims.second;
    std::cout << w << " " << h << std::endl;
    //EnvironmentHandler::instance()->GetDesktopSize( w, h );
    int largeWidth =  w * m; 
    int largeHeight = h * m ;
    shot->allocateImage( largeWidth, largeHeight, 1, GL_RGB, GL_UNSIGNED_BYTE );

    //Now lets create the scene
    osg::ref_ptr< osg::Node > subgraph = new osg::Group( *root );
    osg::ref_ptr< osg::Camera > rttCameraList;
    //Create the screen shot root
    osg::ref_ptr< osg::Group > screenShotRoot = new osg::Group;

    //Create the list of RTT's
    osg::ref_ptr< osg::Texture2D > rttList;

    //osg::ref_ptr<osgUtil::SceneView> sv;
    //sv = ( *sceneViewer );  // Get context specific scene viewer
    osg::ref_ptr<osg::Camera> oldcamera = sv->getCamera();
    //Copy the settings from sceneView-camera to
    //get exactly the view the user sees at the moment:
    //Get the current frustum from the current sceneView-camera
    double frustum[6] = {0, 0, 0, 0, 0, 0};
    oldcamera->getProjectionMatrixAsFrustum(
        frustum[ 0 ], frustum[ 1 ], frustum[ 2 ],
        frustum[ 3 ], frustum[ 4 ], frustum[ 5 ] );
    //Create 4 cameras whose frustums tile the original camera frustum
    //double tileFrustum[6] = { 0, 0, 0, 0, 0, 0 };
    //z values don't change
    //tileFrustum[ 4 ] = frustum[ 4 ];
    //tileFrustum[ 5 ] = frustum[ 5 ];

    osg::ref_ptr< osg::Texture2D > textures;
    //for( size_t i = 0; i < 1; ++i )
    {
        //Set up the RTT's (Render-To-Textures)
        //The output textures here are 2x as big as the desired tile
        //This gives us more information to fight aliasing by "super-sampling"
        //at the desired resolution 
        textures = new osg::Texture2D();
        //textures = new osg::Image();
        //textures->allocateImage( w, h, 1, GL_RGB, GL_UNSIGNED_BYTE );

        //rttList = textures;
        textures->setTextureSize( w * m, h * m );
        textures->setInternalFormat( GL_RGB );
        textures->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
        textures->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
        textures->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE );
        textures->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE );

        //Setup the cameras
        rttCameraList = new osg::Camera();
        rttCameraList->setClearColor( oldcamera->getClearColor() );
        rttCameraList->setClearMask( oldcamera->getClearMask() );
        rttCameraList->setColorMask( oldcamera->getColorMask() );
        rttCameraList->setTransformOrder( oldcamera->getTransformOrder() );
        rttCameraList->setViewMatrix( oldcamera->getViewMatrix() );
        //Set view
        rttCameraList->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
        //Set the camera to render before after the main camera
        rttCameraList->setRenderOrder( osg::Camera::PRE_RENDER );
        //Tell the camera to use OpenGL frame buffer object where supported
        rttCameraList->setRenderTargetImplementation(
            osg::Camera::FRAME_BUFFER_OBJECT );
        //Add subgraph to render
        rttCameraList->addChild( subgraph.get() );

        //Set viewport
        rttCameraList->setViewport( 0, 0, w * m, h * m );

        ///Attach the camera to the image
        rttCameraList->attach(
            osg::Camera::COLOR_BUFFER, textures.get(), 0, 0, false, 8, 8 );
        screenShotRoot->addChild( rttCameraList.get() );
        rttCameraList->setProjectionMatrixAsFrustum(
            frustum[ 0 ], frustum[ 1 ], frustum[ 2 ],
            frustum[ 3 ], frustum[ 4 ], frustum[ 5 ] );
    }

    //Add the screen shot as a pre-render node of the main graph
    root->addChild( screenShotRoot.get() );

    //Render to produce the tiles via RTT
    sv->update();
    sv->cull();
    sv->draw();
    //Remove the screen shot from the graph
    root->removeChild( screenShotRoot.get() );
    shot->copySubImage( 0, 0, 0, textures->getImage() );

    //This would work, too:
    osgDB::writeImageFile( *( shot.get() ), filename );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::SetImageCameraCallback(
    bool capture, const std::string& filename )
{
    //std::pair< int, int > screenDims =
        //EnvironmentHandler::instance()->GetDisplaySettings()->GetScreenResolution();
    //int w = 0; int  h = 0; int m = 1;
    //w = screenDims.first;
    //h = screenDims.second;
    
    for( std::vector< osg::Camera* >::iterator iter = m_updateList.begin(); 
        iter != m_updateList.end(); ++iter )
    {
        osgwTools::ScreenCapture* capTools = m_captureTools[ *iter ].get();
        capTools->setCapture( capture );

        if( capture )
        {
            std::ostringstream ostr;
            ostr << *iter;

            std::string name = filename;
            name += "_";
            name += ostr.str();
            name += "_";

            capTools->setRootName( name );
            (*iter)->setPostDrawCallback( capTools );
        }
        else
        {
            (*iter)->setPostDrawCallback( 0 );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
