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

// --- VE-Suite Includes --- //
#include "SceneRenderToTexture.h"

#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/environment/cfdDisplaySettings.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CameraImageCaptureCallback.h>

#include <ves/xplorer/scenegraph/rtt/Processor.h>
#include <ves/xplorer/scenegraph/rtt/UnitCameraAttachmentBypass.h>
#include <ves/xplorer/scenegraph/rtt/UnitInOut.h>
#include <ves/xplorer/scenegraph/rtt/UnitInResampleOut.h>
#include <ves/xplorer/scenegraph/rtt/UnitOut.h>
//#include <ves/xplorer/scenegraph/rtt/ShaderAttribute.h>
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

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Camera>
#include <osg/ClearNode>
#include <osg/FrameBufferObject>

#include <osgDB/WriteFile>
#include <osgDB/ReaderWriter>
#include <osgDB/ReadFile>

#include <osgUtil/SceneView>
#include <osgUtil/UpdateVisitor>

#include <osgwTools/ScreenCapture.h>

// --- STL Includes --- //
#include <iostream>

//#define VES_SRTT_DEBUG

using namespace ves::xplorer;

namespace vxsr = ves::xplorer::scenegraph::rtt;

////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::SceneRenderToTexture()
    :
    mScaleFactor( 1 ),
    mRootGroup( new osg::Group() )
{
    osg::ref_ptr< osgDB::ReaderWriter::Options > vertexOptions =
        new osgDB::ReaderWriter::Options( "vertex" );
    osg::ref_ptr< osgDB::ReaderWriter::Options > fragmentOptions =
        new osgDB::ReaderWriter::Options( "fragment" );

    try
    {
        m_finalShader = 
            osgDB::readShaderFile( "glsl/final_fp.glsl", fragmentOptions.get() );
    }
    catch( ... )
    {
        std::cerr << "Could not load shader files!" << std::endl;
    }

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

    //Setup the MRT shader to make glow work correctly
    m_topLevelGlow = new osg::Shader();
    std::string fragmentSource =
    "uniform vec4 glowColor; \n"

    "void main() \n"
    "{ \n"
        "glowColor.a = gl_Color.a; \n"

        "gl_FragData[ 0 ] = gl_Color; \n"
        "gl_FragData[ 1 ] = glowColor; \n"
    "} \n";

    m_topLevelGlow->setType( osg::Shader::FRAGMENT );
    m_topLevelGlow->setShaderSource( fragmentSource );
    m_topLevelGlow->setName( "Top Level Glow" );
}
////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::~SceneRenderToTexture()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitScene( osg::Camera* const sceneViewCamera )
{
    if( !ves::xplorer::scenegraph::SceneManager::instance()->IsRTTOn() )
    {
        m_updateList.push_back( sceneViewCamera );
        m_captureTools[ sceneViewCamera ] = 
            new osgwTools::ScreenCapture( "test_image", ".png", true );
        return;
    }

    //Get window and viewport information
    vrj::opengl::DrawManager* glDrawManager = vrj::opengl::DrawManager::instance();
    vrj::opengl::UserData* glUserData = glDrawManager->currentUserData();
    vrj::opengl::WindowPtr glWindow = glUserData->getGlWindow();
    vrj::DisplayPtr display = glWindow->getDisplay();

    //Get state info about the screen
    int contextOriginX, contextOriginY, contextWidth, contextHeight;
    display->getOriginAndSize(
        contextOriginX, contextOriginY, contextWidth, contextHeight );

    std::cout << "|\tContext Screen Dimensions: "
              << " ( " << contextWidth
              << ", "  << contextHeight << " )"
              << std::endl;

    size_t numViewports = display->getNumViewports();

    std::cout << "|\tNumber of Viewports: " << numViewports << std::endl;
    std::cout << "|\t" << std::endl;

    ///The root group that all RTT pipelines are added on
    osg::ref_ptr< osg::Group > rttPipelines = new osg::Group();
    rttPipelines->setCullingActive( false );

    for( size_t i = 0; i < numViewports; ++i )
    {
        std::cout << "|\tViewport " << i + 1 << ": " << std::endl;

        vrj::ViewportPtr viewport = display->getViewport( i );

        //Get state info about the viewport
        float viewportOriginX, viewportOriginY, viewportWidth, viewportHeight;
        viewport->getOriginAndSize(
            viewportOriginX, viewportOriginY, viewportWidth, viewportHeight );

        const unsigned int ll_x = static_cast< unsigned int >(
            viewportOriginX * static_cast< float >( contextWidth ) );
        const unsigned int ll_y = static_cast< unsigned int >(
            viewportOriginY * static_cast< float >( contextHeight ) );
        const unsigned int x_size = static_cast< unsigned int >(
            viewportWidth * static_cast< float >( contextWidth ) );
        const unsigned int y_size = static_cast< unsigned int >(
            viewportHeight * static_cast< float >( contextHeight ) );

        std::cout << "|\tll_x: " << ll_x << std::endl
                  << "|\tll_y: " << ll_y << std::endl
                  << "|\tx_size: " << x_size << std::endl
                  << "|\ty_size: " << y_size << std::endl
                  << "|\t" << std::endl;

        osg::ref_ptr< osg::Viewport > osgViewport = new osg::Viewport();
        osgViewport->setViewport( 0, 0, x_size, y_size );

        osg::ref_ptr< osg::Camera > camera =
            CreatePipelineCamera( osgViewport.get() );
        osg::ref_ptr< vxsr::Processor > processor =
            CreatePipelineProcessor( viewport, camera.get(), sceneViewCamera );

        //Set the bin # to be a large number
        //This allows the transparent geometry to be rendered first
        processor->getOrCreateStateSet()->setRenderBinDetails(
            100, std::string( "RenderBin" ) );

        //Add the scenegraph to the camera
        camera->addChild( mRootGroup.get() );
        rttPipelines->addChild( processor.get() );

        //Setup a post-processing pipeline for each viewport per context
        //Each pipeline consists of a osg::Camera and vxsr::Processor
        (*mPipelines)[ viewport ] =
            std::make_pair( camera.get(), processor.get() );

        sceneViewCamera->addChild( camera.get() );
    }
    sceneViewCamera->addChild( rttPipelines.get() );

    // make sure that existing scene graph objects are 
    // allocated with thread safe ref/unref
    sceneViewCamera->setThreadSafeRefUnref( true );

    // update the scene graph so that it has enough GL object buffer 
    // memory for the graphics contexts that will be using it.
    sceneViewCamera->resizeGLObjectBuffers(
        osg::DisplaySettings::instance()->getMaxNumberOfGraphicsContexts() );

    m_updateList.push_back( sceneViewCamera );
    m_captureTools[ sceneViewCamera ] =
        new osgwTools::ScreenCapture( "test_image", ".png", true );

    *mCamerasConfigured = true;
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* SceneRenderToTexture::CreatePipelineCamera(
    osg::Viewport* viewport )
{
    osg::Camera* tempCamera = new osg::Camera();
    tempCamera->setClearStencil( 0 );
    tempCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
    tempCamera->setRenderOrder( osg::Camera::PRE_RENDER, 0 );
    tempCamera->setClearMask( 
        GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    tempCamera->setClearColor( osg::Vec4( 0.0, 0.0, 1.0, 1.0 ) );
    tempCamera->setRenderTargetImplementation(
        osg::Camera::FRAME_BUFFER_OBJECT, osg::Camera::FRAME_BUFFER );
    tempCamera->setViewport( viewport );
    tempCamera->setViewMatrix( osg::Matrix::identity() );
    tempCamera->setProjectionMatrix( osg::Matrix::identity() );
    tempCamera->setComputeNearFarMode(
        osg::CullSettings::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
    tempCamera->setCullingActive( false );

    std::pair< int, int > viewportDimensions = std::make_pair< int, int >( 
        static_cast< int >( viewport->width() ),
        static_cast< int >( viewport->height() ) );

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
        osg::Texture2D::LINEAR, osg::Texture2D::REPEAT,
        viewportDimensions );

    //Attach a texture and use it as the render target
    tempCamera->attach(
        osg::Camera::COLOR_BUFFER0, colorMap.get() );//, 0, 0, false, 4, 4 );
    tempCamera->attach(
        osg::Camera::COLOR_BUFFER1, glowMap.get() );//, 0, 0, false, 0, 0 );
    //tempCamera->attach(
        //osg::Camera::COLOR_BUFFER2, mGlowStencil.get() );//, 0, 0, false, 0, 0 );

    /*
    //Set up the depth buffer
    osg::ref_ptr< osg::Texture2D > depthMap = CreateViewportTexture(
        GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE,
        osg::Texture2D::NEAREST, osg::Texture2D::CLAMP_TO_EDGE,
        viewportDimensions );
    */

    /*
    //Set up interleaved depth/stencil buffer
    osg::ref_ptr< osg::Texture2D > depthStencilMap = CreateViewportTexture(
        GL_DEPTH24_STENCIL8_EXT, GL_DEPTH_STENCIL_EXT, GL_UNSIGNED_INT_24_8_EXT,
        osg::Texture2D::NEAREST, osg::Texture2D::CLAMP_TO_EDGE,
        viewportDimensions );
    */

    //Use interleaved depth/stencil renderbuffer
    tempCamera->attach(
        osg::Camera::PACKED_DEPTH_STENCIL_BUFFER, GL_DEPTH_STENCIL_EXT );

    //osg::ref_ptr< osg::ClearNode > clearNode = new osg::ClearNode();
    //clearNode->setClearMask( GL_STENCIL_BUFFER_BIT );
    //tempCamera->addChild( clearNode.get() );

    osg::ref_ptr< osg::StateSet > stateset = tempCamera->getOrCreateStateSet();

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( m_topLevelGlow.get() );
    program->setName( "Top Level Glow" );

    stateset->setAttributeAndModes( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    //Default glow color for any children that don't explicitly set it.
    stateset->addUniform(
        new osg::Uniform( "glowColor", osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) ) );

    return tempCamera;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* SceneRenderToTexture::CreateViewportTexture(
    GLenum internalFormat,
    GLenum sourceFormat,
    GLenum sourceType,
    osg::Texture2D::FilterMode filterMode,
    osg::Texture2D::WrapMode wrapMode,
    std::pair< int, int >& viewportDimensions )
{
    //GL_RGBA8/GL_UNSIGNED_INT - GL_RGBA16F_ARB/GL_FLOAT
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
vxsr::Processor* SceneRenderToTexture::CreatePipelineProcessor(
    vrj::ViewportPtr viewport, osg::Camera* camera, osg::Camera* svCamera )
{
    //This is the code for the post-processing pipeline
    vxsr::Processor* tempProcessor = new vxsr::Processor();
    tempProcessor->SetCamera( camera );

    //COLOR_BUFFER0 bypass
    osg::ref_ptr< vxsr::UnitCameraAttachmentBypass > colorBuffer0 =
        new vxsr::UnitCameraAttachmentBypass();
    {
        colorBuffer0->setName( "ColorBuffer0Bypass" );
        colorBuffer0->SetBufferComponent( osg::Camera::COLOR_BUFFER0 );
        colorBuffer0->SetInputTextureIndexForViewportReference( 0 );
    }
    tempProcessor->addChild( colorBuffer0.get() );
    colorBuffer0->Update();

    //COLOR_BUFFER1 bypass
    osg::ref_ptr< vxsr::UnitCameraAttachmentBypass > colorBuffer1 =
         new vxsr::UnitCameraAttachmentBypass();
    {
        colorBuffer1->setName( "ColorBuffer1Bypass" );
        colorBuffer1->SetBufferComponent( osg::Camera::COLOR_BUFFER1 );
        colorBuffer1->SetInputTextureIndexForViewportReference( 0 );
    }
    tempProcessor->addChild( colorBuffer1.get() );
    colorBuffer1->Update();

    //Downsample by 1/2 original size
    osg::Vec2 quadScreenSize(
        camera->getViewport()->width(), camera->getViewport()->height() );
    osg::ref_ptr< vxsr::UnitInResampleOut > glowDownSample =
        new vxsr::UnitInResampleOut();
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
    osg::ref_ptr< vxsr::UnitInOut > blurX = new vxsr::UnitInOut();
    {
        //Set name and indicies
        blurX->setName( "BlurHorizontal" );

        //osg::ref_ptr< vxsr::ShaderAttribute > gaussX =
            //new vxsr::ShaderAttribute();
        osg::ref_ptr< osg::Program > gaussX = new osg::Program();

        //Setup horizontal blur shaders
        gaussX->addShader( m_1dxVP.get() );
        gaussX->addShader( m_1dxFP.get() );
        gaussX->setName( "BlurHorizontalShader" );

        //gaussX->add( "quadScreenSize", osg::Uniform::FLOAT_VEC2 );
        //gaussX->add( "WT9_0", osg::Uniform::FLOAT );
        //gaussX->add( "WT9_1", osg::Uniform::FLOAT );
        //gaussX->add( "WT9_2", osg::Uniform::FLOAT );
        //gaussX->add( "WT9_3", osg::Uniform::FLOAT );
        //gaussX->add( "WT9_4", osg::Uniform::FLOAT );
        //gaussX->add( "glowMap", osg::Uniform::SAMPLER_2D );
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

        //gaussX->set( "quadScreenSize", quadScreenSize );
        //gaussX->set( "WT9_0", static_cast< float >( 0.5 ) );
        //gaussX->set( "WT9_1", static_cast< float >( 0.4 ) );
        //gaussX->set( "WT9_2", static_cast< float >( 0.3 ) );
        //gaussX->set( "WT9_3", static_cast< float >( 0.2 ) );
        //gaussX->set( "WT9_4", static_cast< float >( 0.1 ) );
        //gaussX->set( "glowMap", 0 );

        //blurX->getOrCreateStateSet()->setAttributeAndModes( gaussX.get() );
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
        blurX->SetInputToUniform( glowDownSample.get(), "glowMap", true );
        blurX->SetInputTextureIndexForViewportReference( 0 );
    }
    //glowDownSample->addChild( blurX.get() );
    blurX->Update();

    //Perform vertical 1D gauss convolution
    osg::ref_ptr< vxsr::UnitInOut > blurY = new vxsr::UnitInOut();
    {
        //Set name and indicies
        blurY->setName( "BlurVertical" );

        //osg::ref_ptr< vxsr::ShaderAttribute > gaussY =
            //new vxsr::ShaderAttribute();
        osg::ref_ptr< osg::Program > gaussY = new osg::Program();

        //Setup vertical blur shaders
        gaussY->addShader( m_1dyVP.get() );
        gaussY->addShader( m_1dyFP.get() );
        gaussY->setName( "BlurVerticalShader" );

        //gaussY->add( "quadScreenSize", osg::Uniform::FLOAT_VEC2 );
        //gaussY->add( "WT9_0", osg::Uniform::FLOAT );
        //gaussY->add( "WT9_1", osg::Uniform::FLOAT );
        //gaussY->add( "WT9_2", osg::Uniform::FLOAT );
        //gaussY->add( "WT9_3", osg::Uniform::FLOAT );
        //gaussY->add( "WT9_4", osg::Uniform::FLOAT );
        //gaussY->add( "glowMap", osg::Uniform::SAMPLER_2D );
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

        //gaussY->set( "quadScreenSize", quadScreenSize );
        //gaussY->set( "WT9_0", static_cast< float >( 0.5 ) );
        //gaussY->set( "WT9_1", static_cast< float >( 0.4 ) );
        //gaussY->set( "WT9_2", static_cast< float >( 0.3 ) );
        //gaussY->set( "WT9_3", static_cast< float >( 0.2 ) );
        //gaussY->set( "WT9_4", static_cast< float >( 0.1 ) );
        //gaussY->set( "glowMap", 0 );

        //blurY->getOrCreateStateSet()->setAttributeAndModes( gaussY.get() );
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
        blurY->SetInputToUniform( blurX.get(), "glowMap", true );
        blurY->SetInputTextureIndexForViewportReference( 0 );
    }
    //blurX->addChild( blurY.get() );
    blurY->Update();

    //Perform final color operations and blends
    osg::ref_ptr< vxsr::UnitInOut > final = new vxsr::UnitInOut();
    //vxsr::UnitInOut* final = new vxsr::UnitInOut();
    {
        //Set name and indicies
        final->setName( "Final" );

        //osg::ref_ptr< vxsr::ShaderAttribute > finalShader =
            //new vxsr::ShaderAttribute();
        osg::ref_ptr< osg::Program > finalShader = new osg::Program();

        //Setup vertical blur shaders
        finalShader->addShader( m_finalShader.get() );
        finalShader->setName( "FinalShader" );

        //finalShader->add( "glowStrength", osg::Uniform::FLOAT );
        //finalShader->set( "glowStrength", static_cast< float >( 6.0 ) );

        osg::ref_ptr< osg::Uniform > glowStrengthUniform =
            new osg::Uniform( "glowStrength", static_cast< float >( 6.0 ) );

        //final->getOrCreateStateSet()->setAttributeAndModes( finalShader.get() );
        osg::ref_ptr< osg::StateSet > stateSet = final->getOrCreateStateSet();
        stateSet->setAttribute(
            finalShader.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateSet->addUniform( glowStrengthUniform.get() );
        final->SetInputToUniform( colorBuffer0.get(), "baseMap", false );
        final->SetInputToUniform( colorBuffer1.get(), "stencilMap", false );
        final->SetInputToUniform( blurY.get(), "glowMap", true );
        final->SetInputTextureIndexForViewportReference( 0 );
    }
    final->Update();

    //Render to the Frame Buffer
    svCamera->addChild( CreateTexturedQuad(
        viewport, static_cast< osg::Texture2D* const >(
            final->GetOutputTexture() ) ) );

    return tempProcessor;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* SceneRenderToTexture::CreateTexturedQuad(
    vrj::ViewportPtr viewport, osg::Texture2D* texture )
{
    vrj::SurfaceViewportPtr tempView =
        boost::dynamic_pointer_cast< vrj::SurfaceViewport >( viewport );

    float viewportOriginX, viewportOriginY, viewportWidth, viewportHeight;
    tempView->getOriginAndSize(
        viewportOriginX, viewportOriginY, viewportWidth, viewportHeight );

    /*
    std::cout << viewportOriginX << " "
              << viewportOriginY << " " 
              << viewportWidth << " "
              << viewportHeight << std::endl;
    */

    float lx, ly, ux, uy;
    //Straight mapping from ( 0 to 1 ) viewport space to
    //                      ( 0 to 1 ) ortho projection space
    lx = viewportOriginX;
    ly = viewportOriginY;
    ux = viewportOriginX + viewportWidth;
    uy = viewportOriginY + viewportHeight;

    //Transform ( 0 to 1 ) viewport space into
    //          ( -1 to 1 ) identity projection space
    //lx = ( viewportOriginX * 2.0 ) - 1.0;
    //ly = ( viewportOriginY * 2.0 ) - 1.0;
    //ux = ( ( viewportOriginX + viewportWidth ) * 2.0 ) - 1.0;
    //uy = ( ( viewportOriginY + viewportHeight )* 2.0 ) - 1.0;

    //std::cout << lx << " " << ly << " " << ux << " " << uy << std::endl;

    //Get the vertex coordinates for the quad
    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );

    (*quadVertices)[ 0 ].set( lx, ly, 0.0 );
    (*quadVertices)[ 1 ].set( ux, ly, 0.0 );
    (*quadVertices)[ 2 ].set( ux, uy, 0.0 );
    (*quadVertices)[ 3 ].set( lx, uy, 0.0 );

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

#ifndef VES_SRTT_DEBUG
    quadGeometry->setColorBinding( osg::Geometry::BIND_OFF );
#else
    osg::ref_ptr< osg::Vec4Array > colorArray = new osg::Vec4Array();
    colorArray->push_back( osg::Vec4f( 1.0, 1.0, 0.0, 1.0 ) );
    quadGeometry->setColorArray( colorArray.get() );
    quadGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );
#endif

    //Create geode for quad
    osg::Geode* quadGeode = new osg::Geode();
    quadGeode->setCullingActive( false );
    quadGeode->addDrawable( quadGeometry.get() );

    //Set stateset for quad
    osg::ref_ptr< osg::StateSet > stateset = quadGeode->getOrCreateStateSet();
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

#ifndef VES_SRTT_DEBUG
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    std::string fragmentSource =
    "uniform sampler2D baseMap; \n"

    "void main() \n"
    "{ \n"
        "vec3 base = texture2D( baseMap, gl_TexCoord[ 0 ].xy ).rgb; \n"

        "gl_FragData[ 0 ] = vec4( base, 1.0 ); \n"
    "} \n";

    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );
    fragmentShader->setName( "VS Quad Fragment Shader" );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragmentShader.get() );
    program->setName( "VS Quad Program" );

    stateset->setAttributeAndModes(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->addUniform( new osg::Uniform( "baseMap", 0 ) );
    stateset->setTextureAttributeAndModes( 0, texture, osg::StateAttribute::ON );
#endif

    //This doesn't make sense? !jbkoch
#ifdef VES_SRTT_DEBUG
    quadGeode->setNodeMask( 0 );
#endif

    return quadGeode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* const SceneRenderToTexture::GetGroup() const
{
    return mRootGroup.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* const SceneRenderToTexture::GetCamera(
    vrj::ViewportPtr const viewport )
{
    PipelineMap::const_iterator itr = (*mPipelines).find( viewport );
    if( itr != (*mPipelines).end() )
    {
        return itr->second.first.get();
    }
    else
    {
        std::cout << "SceneRenderToTexture::GetCamera: camera not found!"
                  << std::endl;

        return NULL;
    }
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
        float scoord = 1.0 / (float)( w * 2 );
        float tcoord = 1.0 / (float)( h * 2 );
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
    return *mCamerasConfigured;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitializeRTT()
{
    *mCamerasConfigured = false;
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
/*
void SceneRenderToTexture::ConfigureRTTCameras()
{
    if( !(*mCamerasConfigured) )
    {
        return;
    }

    vrj::opengl::DrawManager* glDrawManager =
        vrj::opengl::DrawManager::instance();
    vrj::opengl::UserData* glUserData = glDrawManager->currentUserData();
    vrj::opengl::WindowPtr glWindow = glUserData->getGlWindow();

    vrj::DisplayPtr display = glWindow->getDisplay();
    const size_t numViewports = display->getNumViewports();

    // --- FOR EACH VIEWPORT -- //
    vrj::Viewport::View view;
    float vp_ox, vp_oy, vp_sx, vp_sy; //Viewport origin and size
    for( size_t i = 0; i < numViewports; ++i )
    {
        vrj::ViewportPtr viewport = display->getViewport( i );

        //Should viewport be rendered???
        if( viewport->isActive() )
        {            
            //Set the glViewport to draw within
            viewport->getOriginAndSize( vp_ox, vp_oy, vp_sx, vp_sy );
            glWindow->setViewport( vp_ox, vp_oy, vp_sx, vp_sy );
            
            //Set user information
            glUserData->setUser( viewport->getUser() );       //Set user data
            glUserData->setViewport( viewport );              //Set the viewport
            //std::cout << view << std::endl;
            // ---- SURFACE & Simulator --- //
            {
                //The view for the active viewport
                view = viewport->getView();
                if( ( vrj::Viewport::STEREO == view ) ||
                    ( vrj::Viewport::LEFT_EYE == view ) )     //LEFT EYE
                {
                    glWindow->setViewBuffer( vrj::Viewport::LEFT_EYE );
                    glWindow->setProjection( viewport->getLeftProj() );
                    glUserData->setProjection( viewport->getLeftProj() );

                    //Update rtt camera
                    UpdateRTTQuadAndViewport();
                }

                if( ( vrj::Viewport::STEREO == view ) ||
                    ( vrj::Viewport::RIGHT_EYE == view ) )    //RIGHT EYE
                {
                    glWindow->setViewBuffer( vrj::Viewport::RIGHT_EYE );
                    glWindow->setProjection( viewport->getRightProj() );
                    glUserData->setProjection( viewport->getRightProj() );

                    //Update rtt camera
                    UpdateRTTQuadAndViewport();
                }
            }
        }
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
void SceneRenderToTexture::UpdateRTTQuadAndViewport()
{
    vrj::ViewportPtr viewport = vrj::opengl::DrawManager::instance()->
        currentUserData()->getViewport();

    PipelineMap::iterator itr = (*mPipelines).find( viewport );
    if( itr != (*mPipelines).end() )
    {
        PipelinePair* activePipeline = &(itr->second);

        //Get the frustrum
        vrj::ProjectionPtr project = vrj::opengl::DrawManager::instance()->
            currentUserData()->getProjection();

        vrj::Frustum frustum = project->getFrustum();

        activePipeline->first->setProjectionMatrixAsFrustum(
            frustum[ vrj::Frustum::VJ_LEFT ], frustum[ vrj::Frustum::VJ_RIGHT ],
            frustum[ vrj::Frustum::VJ_BOTTOM ], frustum[ vrj::Frustum::VJ_TOP ],
            frustum[ vrj::Frustum::VJ_NEAR ], frustum[ vrj::Frustum::VJ_FAR ] );

        gmtl::Vec3f x_axis( 1.0f, 0.0f, 0.0f );
        gmtl::Matrix44f mZUp = gmtl::makeRot< gmtl::Matrix44f >( 
            gmtl::AxisAnglef( gmtl::Math::deg2Rad( -90.0f ), x_axis ) );
        gmtl::Matrix44f vjMatrixLeft( project->getViewMatrix() );
        gmtl::Matrix44f mNavPosition =  gmtl::convertTo< float >( 
            ves::xplorer::scenegraph::SceneManager::instance()->
                GetActiveNavSwitchNode()->GetMat() );

        //Transform into z-up land
        vjMatrixLeft = vjMatrixLeft * mZUp * mNavPosition;
        osg::ref_ptr< osg::RefMatrix > osg_proj_xform_mat =
            new osg::RefMatrix();
        osg_proj_xform_mat->set( vjMatrixLeft.mData );
        activePipeline->first->setViewMatrix( *(osg_proj_xform_mat.get()) );
        //vprDEBUG( vesDBG, 1 )
            //<< vjMatrixLeft << mZUp << mNavPosition << std::endl
            //<< frustum << std::endl << std::endl << vprDEBUG_FLUSH;
    }
    else
    {
        std::cout << "SceneRenderToTexture::UpdateRTTQuadAndViewport: "
                  << viewport->getName() << "viewport not found!" << std::endl;
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
