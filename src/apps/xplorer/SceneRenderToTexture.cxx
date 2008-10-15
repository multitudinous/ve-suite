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

// --- VE-Suite Includes --- //
#include "SceneRenderToTexture.h"

#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/environment/cfdDisplaySettings.h>

// ---  VR Juggler Includes --- //
#include <vrj/Draw/OGL/GlWindow.h>
#include <vrj/Draw/OGL/GlDrawManager.h>

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/Group>
#include <osg/ClearNode>
#include <osg/Texture2D>
#include <osg/FrameBufferObject>

#include <osgDB/WriteFile>
#include <osgDB/ReaderWriter>
#include <osgDB/ReadFile>

#include <osgUtil/SceneView>
#include <osgUtil/UpdateVisitor>

#include <osgPPU/Processor.h>
#include <osgPPU/Unit.h>
#include <osgPPU/UnitInOut.h>
#include <osgPPU/UnitText.h>
#include <osgPPU/UnitInResampleOut.h>
#include <osgPPU/UnitInMipmapOut.h>
#include <osgPPU/UnitOut.h>
#include <osgPPU/UnitOutCapture.h>
#include <osgPPU/UnitBypass.h>
#include <osgPPU/UnitTexture.h>
#include <osgPPU/UnitCameraAttachmentBypass.h>
#include <osgPPU/UnitDepthbufferBypass.h>
#include <osgDB/ReaderWriter>
#include <osgDB/ReadFile>
#include <osgPPU/ShaderAttribute.h>

// --- C/C++ Libraries --- //
#include <iostream>

//jbkoch - these do not seem to be defined in osg
#ifndef GL_DEPTH_STENCIL_EXT
#define GL_DEPTH_STENCIL_EXT 0x84F9
#endif//GL_DEPTH_STENCIL_EXT
#ifndef GL_UNSIGNED_INT_24_8_EXT
#define GL_UNSIGNED_INT_24_8_EXT 0x84FA
#endif//GL_UNSIGNED_INT_24_8_EXT
#ifndef GL_DEPTH24_STENCIL8_EXT
#define GL_DEPTH24_STENCIL8_EXT 0x88F0
#endif//GL_DEPTH24_STENCIL8_EXT
#ifndef GL_TEXTURE_STENCIL_SIZE_EXT
#define GL_TEXTURE_STENCIL_SIZE_EXT 0x88F1
#endif//GL_TEXTURE_STENCIL_SIZE_EXT

using namespace ves::xplorer;

/*
////////////////////////////////////////////////////////////////////////////////
StencilImage::StencilImage()
    :
    _image( new osg::Image() )
{
    ;       
}
////////////////////////////////////////////////////////////////////////////////
void StencilImage::operator () ( osg::RenderInfo& renderInfo ) const
{
    osg::notify( osg::NOTICE )<< "Camera callback" << std::endl;

    osg::Camera* camera = renderInfo.getCurrentCamera();
    osg::Viewport* viewport = camera ? camera->getViewport() : 0;

    osg::notify( osg::NOTICE ) << "Camera callback " << camera
                               << " " << viewport << std::endl;

    if( viewport && _image.valid() )
    {
        _image->readPixels( int( viewport->x() ), int( viewport->y() ),
                            int( viewport->width() ), int( viewport->height() ),
                            GL_DEPTH_STENCIL_EXT, GL_UNSIGNED_INT_24_8_EXT );
        osgDB::writeImageFile( *_image, "C:/TSVEG/stencil.jpg" );
        
        osg::notify( osg::NOTICE ) << "Taken screenshot, and written to '"
                                   << "C:/TSVEG/stencil.jpg" << "'" << std::endl;
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::SceneRenderToTexture()
    :
    mRootGroup( new osg::Group() ),
    mScaleFactor( 1 )
{    
    ;
}
////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::~SceneRenderToTexture()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitTextures( std::pair< int, int >& screenDims )
{
    *mColorMap = new osg::Texture2D();
    osg::ref_ptr< osg::Texture2D > tempColorMap = (*mColorMap).get();
    //GL_RGBA8/GL_UNSIGNED_INT - GL_RGBA16F_ARB/GL_FLOAT 
    tempColorMap->setInternalFormat( GL_RGBA16F_ARB );
    tempColorMap->setTextureSize(
        screenDims.first * mScaleFactor, screenDims.second * mScaleFactor );
    tempColorMap->setSourceFormat( GL_RGBA );
    tempColorMap->setSourceType( GL_FLOAT );
    tempColorMap->setFilter(
        osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    tempColorMap->setFilter(
        osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    tempColorMap->setWrap(
        osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    tempColorMap->setWrap(
        osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    *mGlowMap = new osg::Texture2D();
    osg::ref_ptr< osg::Texture2D > tempGlowMap = (*mGlowMap).get();
    //GL_RGBA8/GL_UNSIGNED_INT - GL_RGBA16F_ARB/GL_FLOAT 
    tempGlowMap->setInternalFormat( GL_RGBA16F_ARB );
    tempGlowMap->setTextureSize(
        screenDims.first * mScaleFactor, screenDims.second * mScaleFactor );
    tempGlowMap->setSourceFormat( GL_RGBA );
    tempGlowMap->setSourceType( GL_FLOAT );
    tempGlowMap->setFilter(
        osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    tempGlowMap->setFilter(
        osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    //We really want REPEAT otherwise the edge gets sampled by the glow shader
    tempGlowMap->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::REPEAT );
    tempGlowMap->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::REPEAT );

    /*
    mGlowStencil = new osg::Texture2D();
    //GL_RGBA8/GL_UNSIGNED_INT - GL_RGBA16F_ARB/GL_FLOAT 
    mGlowStencil->setInternalFormat( GL_RGBA8 );
    mGlowStencil->setTextureSize(
        screenDims.first * mScaleFactor, screenDims.second * mScaleFactor );
    mGlowStencil->setSourceFormat( GL_RGBA );
    mGlowStencil->setSourceType( GL_UNSIGNED_INT );
    mGlowStencil->setFilter(
        osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    mGlowStencil->setFilter(
        osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    mGlowStencil->setWrap(
        osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    mGlowStencil->setWrap(
        osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    mDepthStencilTexture = new osg::Texture2D();
    mDepthStencilTexture->setInternalFormat( GL_DEPTH24_STENCIL8_EXT );
    mDepthStencilTexture->setTextureSize(
        screenDims.first * mScaleFactor, screenDims.second * mScaleFactor );
    mDepthStencilTexture->setSourceFormat( GL_DEPTH_STENCIL_EXT );
    mDepthStencilTexture->setSourceType( GL_UNSIGNED_INT_24_8_EXT );
    mDepthStencilTexture->setFilter(
        osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    mDepthStencilTexture->setFilter(
        osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    mDepthStencilTexture->setWrap(
        osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    mDepthStencilTexture->setWrap(
        osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    */
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitCamera( std::pair< int, int >& screenDims )
{
    osg::ref_ptr< osg::Camera > tempCamera = (*mCameraMap).get();
    tempCamera->setReferenceFrame( osg::Camera::RELATIVE_RF );
    tempCamera->setRenderOrder( osg::Camera::PRE_RENDER, 0 );
    tempCamera->setClearMask( 
        GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );// | GL_STENCIL_BUFFER_BIT );
    tempCamera->setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) );
    tempCamera->setRenderTargetImplementation(
        osg::Camera::FRAME_BUFFER_OBJECT );
    tempCamera->setViewport( 0, 0,
        screenDims.first * mScaleFactor, screenDims.second * mScaleFactor );
    
    //Attach a texture and use it as the render target
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 6 ) && ( OSG_VERSION_PATCH >= 0 ) )
    tempCamera->attach(
        osg::Camera::COLOR_BUFFER0, (*mColorMap).get() );//, 0, 0, false, 8, 8 );
    tempCamera->attach(
        osg::Camera::COLOR_BUFFER1, (*mGlowMap).get() );//, 0, 0, false, 8, 8 );
    //mCamera->attach(
    //    osg::Camera::COLOR_BUFFER2, mGlowStencil.get() );//, 0, 0, false, 8, 8 );
#else
    tempCamera->attach( osg::Camera::COLOR_BUFFER0, (*mColorMap).get() );
    tempCamera->attach( osg::Camera::COLOR_BUFFER1, (*mGlowMap).get() );
    //mCamera->attach( osg::Camera::COLOR_BUFFER2, mGlowStencil.get() );
#endif

    //Use an interleaved depth/stencil texture to get a depth and stencil buffer
    //jbkoch: In order to get the stencil buffer to work on my card/driver,
    //jbkoch: the depth and stencil must be attached to the same texture or
    //jbkoch: renderbuffer. I have not found a way to access the renderbuffer
    //jbkoch: for osg::Camera so must create texture for now.
    //mCamera->attach( osg::Camera::DEPTH_BUFFER, mDepthStencilTexture.get() );
    //mCamera->attach( osg::Camera::STENCIL_BUFFER, mDepthStencilTexture.get() );

    //Use renderbuffers to get a depth and stencil buffer
    //mCamera->attach( osg::Camera::DEPTH_BUFFER, GL_DEPTH_COMPONENT24 );
    //mCamera->attach( osg::Camera::STENCIL_BUFFER, GL_STENCIL_INDEX );
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 6 ) && ( OSG_VERSION_PATCH >= 0 ) )
    //mCamera->setClearStencil( 0 );
    //glStencilMask( 0xFFFFFFFF );
#endif

    //This camera has a RELATIVE_RF
    //Therefore the transform is cumulative from parents transforms
    tempCamera->setViewMatrix( osg::Matrix::identity() );
    tempCamera->setProjectionMatrix( osg::Matrix::identity() );

    //There seems to be a problem with sceneView overwriting RTT camera's
    //mask values for GL_STENCIL_BUFFER_BIT
    //osg::ref_ptr< osg::ClearNode > clearNode = new osg::ClearNode();
    //clearNode->setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 0.0 ) );
    //clearNode->setClearMask(
        //GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    //mCamera->addChild( clearNode.get() );

    //ves::xplorer::StencilImage* stencilImage = new ves::xplorer::StencilImage();
    //mCamera->setFinalDrawCallback( stencilImage );

    //Call this when changing images in the fbo
    //The setUpCamera does not run since the camera has the
    //previous RenderStage as a cached object for efficiency issues.
    //If you change the attachment, you should set the camera cache to NULL,
    //then when the rendering occurs, a new RenderStage will be created
    //for the camera and the runCameraSetup will be called again.
    //mCamera->setRenderingCache( NULL );

    //Setup the MRT shader to make glow work correctly
    std::string fragmentSource =
    "uniform vec4 glowColor; \n"

    "void main() \n"
    "{ \n"
        "gl_FragData[ 0 ] = gl_Color; \n"

        "vec4 color = glowColor; \n"
        "if( gl_Color.a < 1.0 ) \n"
        "{ \n"
           "color.a = gl_Color.a; \n"
        "} \n"

        "gl_FragData[ 1 ] = color; \n"
        
    "} \n";

    osg::ref_ptr< osg::StateSet > stateset = tempCamera->getOrCreateStateSet();
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragmentShader.get() );

    stateset->setAttributeAndModes( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    //Default glow color for any children that don't explicitly set it.
    stateset->addUniform(
        new osg::Uniform( "glowColor", osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitProcessor(
    std::pair< int, int >& screenDims, osg::Camera* const sceneViewCamera )
{
    //This is the code for the glow pipeline
    osg::ref_ptr< osgDB::ReaderWriter::Options > vertexOptions =
        new osgDB::ReaderWriter::Options( "vertex" );
    osg::ref_ptr< osgDB::ReaderWriter::Options > fragmentOptions =
        new osgDB::ReaderWriter::Options( "fragment" );

    //COLOR_BUFFER0 bypass
    osg::ref_ptr< osgPPU::UnitCameraAttachmentBypass > colorBuffer0 =
        new osgPPU::UnitCameraAttachmentBypass();
    {
        colorBuffer0->setName( "ColorBuffer0Bypass" );
        colorBuffer0->setBufferComponent( osg::Camera::COLOR_BUFFER0 );
        
    }
    (*mProcessor)->addChild( colorBuffer0.get() );

    //COLOR_BUFFER1 bypass
    osg::ref_ptr< osgPPU::UnitCameraAttachmentBypass > colorBuffer1 =
        new osgPPU::UnitCameraAttachmentBypass();
    {
        colorBuffer1->setName( "ColorBuffer1Bypass" );
        colorBuffer1->setBufferComponent( osg::Camera::COLOR_BUFFER1 );
        
    }
    (*mProcessor)->addChild( colorBuffer1.get() );

    //Downsample by 1/2 original size
    osg::Vec2 quadScreenSize( screenDims.first, screenDims.second );
    osg::ref_ptr< osgPPU::UnitInResampleOut > glowDownSample =
        new osgPPU::UnitInResampleOut();
    {
        float downsample = 0.5;
        quadScreenSize *= downsample;
        
        glowDownSample->setName( "GlowDownSample" );
        glowDownSample->setFactorX( downsample );
        glowDownSample->setFactorY( downsample );
    }
    colorBuffer1->addChild( glowDownSample.get() );

    //Perform horizontal 1D gauss convolution
    osg::ref_ptr< osgPPU::UnitInOut > blurX = new osgPPU::UnitInOut();
    {
        //Set name and indicies
        blurX->setName( "BlurHorizontal" );

        osg::ref_ptr< osgPPU::ShaderAttribute > gaussX =
            new osgPPU::ShaderAttribute();
        osg::ref_ptr< osg::Shader > vhShader, fhShader;
        try
        {
            vhShader = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dx_vp.glsl",
                vertexOptions.get() );
            fhShader = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dx_fp.glsl",
                fragmentOptions.get() );
        }
        catch( ... )
        {
            std::cerr << "Could not load shader files!" << std::endl;
        }

        //Setup horizontal blur shaders
        gaussX->addShader( vhShader.get() );
        gaussX->addShader( fhShader.get() );
        gaussX->setName( "BlurHorizontalShader" );

        gaussX->add( "quadScreenSize", osg::Uniform::FLOAT_VEC2 );
        gaussX->add( "WT9_0", osg::Uniform::FLOAT );
        gaussX->add( "WT9_1", osg::Uniform::FLOAT );
        gaussX->add( "WT9_2", osg::Uniform::FLOAT );
        gaussX->add( "WT9_3", osg::Uniform::FLOAT );
        gaussX->add( "WT9_4", osg::Uniform::FLOAT );
        gaussX->add( "glowMap", osg::Uniform::SAMPLER_2D );

        gaussX->set( "quadScreenSize", quadScreenSize );
        gaussX->set( "WT9_0", static_cast< float >( 0.5 ) );
        gaussX->set( "WT9_1", static_cast< float >( 0.4 ) );
        gaussX->set( "WT9_2", static_cast< float >( 0.3 ) );
        gaussX->set( "WT9_3", static_cast< float >( 0.2 ) );
        gaussX->set( "WT9_4", static_cast< float >( 0.1 ) );
        gaussX->set( "glowMap", 0 );

        blurX->getOrCreateStateSet()->setAttributeAndModes( gaussX.get() );
    }
    glowDownSample->addChild( blurX.get() );

    //Perform vertical 1D gauss convolution
    osg::ref_ptr< osgPPU::UnitInOut > blurY = new osgPPU::UnitInOut();
    {
        //Set name and indicies
        blurY->setName( "BlurVertical" );

        osg::ref_ptr< osgPPU::ShaderAttribute > gaussY =
            new osgPPU::ShaderAttribute();
        osg::ref_ptr< osg::Shader > vvShader, fvShader;
        try
        {
            vvShader = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dy_vp.glsl",
                vertexOptions.get() );
            fvShader = osgDB::readShaderFile(
                "glsl/gauss_convolution_1Dy_fp.glsl",
                fragmentOptions.get() );
        }
        catch( ...  )
        {
            std::cerr << "Could not load shader files!" << std::endl;
        }

        //Setup vertical blur shaders
        gaussY->addShader( vvShader.get() );
        gaussY->addShader( fvShader.get() );
        gaussY->setName( "BlurVerticalShader" );

        gaussY->add( "quadScreenSize", osg::Uniform::FLOAT_VEC2 );
        gaussY->add( "WT9_0", osg::Uniform::FLOAT );
        gaussY->add( "WT9_1", osg::Uniform::FLOAT );
        gaussY->add( "WT9_2", osg::Uniform::FLOAT );
        gaussY->add( "WT9_3", osg::Uniform::FLOAT );
        gaussY->add( "WT9_4", osg::Uniform::FLOAT );
        gaussY->add( "glowMap", osg::Uniform::SAMPLER_2D );

        gaussY->set( "quadScreenSize", quadScreenSize );
        gaussY->set( "WT9_0", static_cast< float >( 0.5 ) );
        gaussY->set( "WT9_1", static_cast< float >( 0.4 ) );
        gaussY->set( "WT9_2", static_cast< float >( 0.3 ) );
        gaussY->set( "WT9_3", static_cast< float >( 0.2 ) );
        gaussY->set( "WT9_4", static_cast< float >( 0.1 ) );
        gaussY->set( "glowMap", 0 );

        blurY->getOrCreateStateSet()->setAttributeAndModes( gaussY.get() );
    }
    blurX->addChild( blurY.get() );

    //Perform final color operations and blends
    osg::ref_ptr< osgPPU::UnitInOut > final = new osgPPU::UnitInOut();
    {
        //Set name and indicies
        final->setName( "Final" );

        osg::ref_ptr< osgPPU::ShaderAttribute > finalShader =
            new osgPPU::ShaderAttribute();
        osg::ref_ptr< osg::Shader > vShader;
        try
        {
            vShader = osgDB::readShaderFile(
                "glsl/final_fp.glsl", fragmentOptions.get() );
        }
        catch( ... )
        {
            std::cerr << "Could not load shader files!" << std::endl;
        }

        //Setup vertical blur shaders
        finalShader->addShader( vShader.get() );
        finalShader->setName( "FinalShader" );

        finalShader->add( "glowStrength", osg::Uniform::FLOAT );
        finalShader->add( "glowColor", osg::Uniform::FLOAT_VEC4 );

        finalShader->set( "glowStrength", static_cast< float >( 4.0 ) );
        finalShader->set(
            "glowColor", osg::Vec4( 0.57255, 1.0, 0.34118, 1.0 ) );

        final->getOrCreateStateSet()->setAttributeAndModes( finalShader.get() );
        final->setInputToUniform( colorBuffer0.get(), "baseMap", true );
        final->setInputToUniform( colorBuffer1.get(), "stencilGlowMap", true );
        final->setInputToUniform( blurY.get(), "glowMap", true );
        final->setInputTextureIndexForViewportReference( -1 );
   }

    //Render to the Frame Buffer
    *mQuadOut = new osgPPU::UnitOut();
    osg::ref_ptr< osgPPU::UnitOut > ppuOut = (*mQuadOut).get();
    {
        ppuOut->setName( "PipelineResult" );
        ppuOut->setInputTextureIndexForViewportReference( -1 );
    }
    final->addChild( ppuOut.get() );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitScene( osg::Camera* const sceneViewCamera )
{
    //Get state info about the screen
    int originX, originY, width, height;
    vrj::GlDrawManager::instance()->currentUserData()->getGlWindow()->
        getDisplay()->getOriginAndSize( originX, originY, width, height );
    size_t numViewports = vrj::GlDrawManager::instance()->currentUserData()->
        getGlWindow()->getDisplay()->getNumViewports();
    float xOrigin, yOrigin, widthRatio, heightRatio;
    float maxWidth, maxHeight;
    maxWidth = 0;
    maxHeight = 0;
    for( size_t i = 0; i < numViewports; ++i )
    {
#if __VJ_version >= 2003000
        vrj::ViewportPtr viewport = vrj::GlDrawManager::instance()->
            currentUserData()->getGlWindow()->getDisplay()->getViewport( i );
#else
        vrj::Viewport* viewport = vrj::GlDrawManager::instance()->
            currentUserData()->getGlWindow()->getDisplay()->getViewport( i );
#endif
        viewport->getOriginAndSize( xOrigin, yOrigin, widthRatio, heightRatio );

        if( maxWidth < widthRatio )
        {
            maxWidth = widthRatio;
        }
        
        if( maxHeight < heightRatio )
        {
            maxHeight = heightRatio;
        }
    }
    width *= maxWidth;
    height *= maxHeight;
    
    std::cout << "|\tRTT Texture Size - Width = " 
        << width << " - Height = " << height << std::endl;

    //Setup cameras, textures, and everything else for rtt
    std::pair< int, int > screenDims = 
        std::make_pair< int, int >( width, height );
    *mCameraMap = new osg::Camera();
    *mProcessor = new osgPPU::Processor();
    
    //Create textures, camera, and SA-quad
    InitTextures( screenDims );
    InitCamera( screenDims );
    InitProcessor( screenDims, sceneViewCamera );

    //Add nodes to the scenegraph
    (*mCameraMap)->addChild( mRootGroup.get() );
    sceneViewCamera->addChild( (*mCameraMap).get() );
    sceneViewCamera->addChild( (*mProcessor).get() );

    (*mProcessor)->setCamera( (*mCameraMap).get() );
    (*mProcessor)->setName( "Processor" );
    (*mProcessor)->useHDR( false );
    (*mProcessor)->dirtyUnitSubgraph();
    
    *mCamerasConfigured = true;
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::UpdateRTTProjectionAndViewportMatrix(
    osgUtil::SceneView* sv )
{
    if( !(*mCamerasConfigured) )
    {
        return;
    }
    
    osg::Camera* svCamera = sv->getCamera();
    (*mQuadOut)->setViewport( svCamera->getViewport() );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::UpdateProcessorAndUnits()
{
    if( !(*mCamerasConfigured) )
    {
        return;
    }

    osg::ref_ptr< osgUtil::UpdateVisitor > update =
        new osgUtil::UpdateVisitor();
    (*mProcessor)->accept( *(update.get()) );
}
////////////////////////////////////////////////////////////////////////////////
osg::Camera* const SceneRenderToTexture::GetCamera()
{
    return (*mCameraMap).get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* const SceneRenderToTexture::GetGroup() const
{
    return mRootGroup.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* const SceneRenderToTexture::GetColorMap()
{
    return (*mColorMap).get();
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
    osg::ref_ptr<osg::Node> subgraph = new osg::Group( *root );
    std::vector< osg::ref_ptr< osg::Camera > > rttCameraList;
    
    ///create the screen shot root
    osg::ref_ptr< osg::Group > screenShotRoot = new osg::Group;
    
    ///create the list of RTT's
    std::vector< osg::ref_ptr< osg::Texture2D > >rttList;
    
    //osg::ref_ptr<osgUtil::SceneView> sv;
    //sv = ( *sceneViewer );  // Get context specific scene viewer
    /* this doesn't seem like the right place for this but
     sceneview isn't exposed anywhere else
     sv->setLODScale( EnvironmentHandler::instance()->GetGlobalLODScale() );
     */
    osg::ref_ptr<osg::Camera> oldcamera = sv->getCamera();
    //Copy the settings from sceneView-camera to
    //get exactly the view the user sees at the moment:
    //Get the current frustum from the current sceneView-camera
    double frustum[6] = {0, 0, 0, 0, 0, 0};
    oldcamera->getProjectionMatrixAsFrustum(
        frustum[0], frustum[1], frustum[2], frustum[3], frustum[4], frustum[5] );
    //Create 4 cameras whose frustums tile the original camera frustum
    double tileFrustum[6] = {0, 0, 0, 0, 0, 0};
    //z values don't change
    tileFrustum[4] = frustum[4];
    tileFrustum[5] = frustum[5];
    
    std::vector< osg::ref_ptr< osg::Texture2D > > textures;
    for( size_t i = 0; i < 4; ++i )
    {
        //Set up the RTT's (Render-To-Textures)
        //The output textures here are 2x as big as the desired tile
        //This gives us more information to fight aliasing by "super-sampling"
        //at the desired resolution 
        rttList.push_back( new osg::Texture2D( ) );
        rttList.back()->setTextureSize( w*2, h*2 );
        rttList.back()->setInternalFormat(GL_RGB);
        rttList.back()->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::LINEAR);
        rttList.back()->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::LINEAR);
        rttList.back()->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
        rttList.back()->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);
        
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
        rttCameraList.back()->attach( osg::Camera::COLOR_BUFFER,
                                     rttList.back().get() );
        screenShotRoot->addChild( rttCameraList.back().get() );
    }
    
    std::vector< osg::ref_ptr<osg::Camera> >::iterator activeCamera;
    ///
    {
        //setup ll
        activeCamera = rttCameraList.begin();
        //left
        tileFrustum[0] = frustum[0];
        //right
        tileFrustum[1] = frustum[0] + ( frustum[1] - frustum[0] ) * .5;
        //bottom
        tileFrustum[2] = frustum[2];
        //top
        tileFrustum[3] = frustum[3] + ( frustum[2] - frustum[3] ) * .5;
        ( *activeCamera )->setProjectionMatrixAsFrustum( tileFrustum[0],
                                                        tileFrustum[1], tileFrustum[2], tileFrustum[3], tileFrustum[4],
                                                        tileFrustum[5] );
    }
    ///
    {
        //setup lr
        activeCamera = rttCameraList.begin() + 1;
        //left
        tileFrustum[0] = frustum[0] + .5 * ( frustum[1] - frustum[0] );
        //right
        tileFrustum[1] = frustum[1];
        //bottom
        tileFrustum[2] = frustum[2];
        //top
        tileFrustum[3] = frustum[3] + ( frustum[2] - frustum[3] ) * .5;
        ( *activeCamera )->setProjectionMatrixAsFrustum( tileFrustum[0],
                                                        tileFrustum[1], tileFrustum[2], tileFrustum[3], tileFrustum[4],
                                                        tileFrustum[5] );
    }
    ///
    {
        //setup ur
        activeCamera = rttCameraList.begin() + 2;
        //left
        tileFrustum[0] = frustum[0] + .5 * ( frustum[1] - frustum[0] );
        //right
        tileFrustum[1] = frustum[1];
        //bottom
        tileFrustum[2] = frustum[3] + ( frustum[2] - frustum[3] ) * .5;
        //top
        tileFrustum[3] = frustum[3];
        ( *activeCamera )->setProjectionMatrixAsFrustum( tileFrustum[0],
                                                        tileFrustum[1], tileFrustum[2], tileFrustum[3], tileFrustum[4],
                                                        tileFrustum[5] );
    }
    ///
    {
        //setup ul
        activeCamera = rttCameraList.begin() + 3;
        //left
        tileFrustum[0] = frustum[0];
        //right
        tileFrustum[1] = frustum[0] + .5 * ( frustum[1] - frustum[0] );
        //bottom
        tileFrustum[2] = frustum[3] + ( frustum[2] - frustum[3] ) * .5;
        //top
        tileFrustum[3] = frustum[3];
        ( *activeCamera )->setProjectionMatrixAsFrustum( tileFrustum[0],
                                                        tileFrustum[1], tileFrustum[2], tileFrustum[3], tileFrustum[4],
                                                        tileFrustum[5] );
    }
    //Add the screen shot as a pre-render node of the main
    //graph. 
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
    "  vec4 sum = texture2D(baseTexture,gl_TexCoord[0].xy );\n"
    "  //if( ( gl_TexCoord[0].x > 0.0 ) &&\n"
    "  //    ( gl_TexCoord[0].x < 1.0 ) &&\n"
    "  //    ( gl_TexCoord[0].y > 0.0 ) &&\n"
    "  //    ( gl_TexCoord[0].y < 1.0 ) )\n"
    "    {\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy + sample);\n"
    "        vec2 diff = vec2(-sample.x, sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy + diff);\n"
    
    "        diff = vec2(-sample.x, -sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy + diff);\n"
    
    "        diff = vec2(sample.x, -sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy + diff);\n"
    
    "        diff = vec2(sample.x, 0.0);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy + diff);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy - diff);\n"
    
    "        diff = vec2(0.0, sample.y);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy + diff);\n"
    "        sum += texture2D(baseTexture,gl_TexCoord[0].xy - diff);\n"
    
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
        fullScreenQuads.back()->getStateSet()->addUniform( new osg::Uniform ( "baseTexture", static_cast<int>(i) ) );
        fullScreenQuads.back()->getStateSet()->addUniform( new osg::Uniform ( "dimensions", osg::Vec2(w * 2,h * 2) ) );
        fullScreenQuads.back()->getStateSet()->setAttribute( ssaaProgram.get() );
        float scoord = 1.0/(float)(w*2);
        float tcoord = 1.0/(float)(h*2);
        fullScreenQuads.back()->addDrawable( osg::createTexturedQuadGeometry( osg::Vec3( -1.0f, -1.0f, -1.0f ),
                                                                              osg::Vec3( 2.0f, 0.0f, 0.0f ),
                                                                              osg::Vec3( 0.0f, 2.0f, 0.0f ),
                                                                              0.0f, 0.0f, 1.0f, 1.0f ) );
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
        fullScreenQuadCameraList.back()->setReferenceFrame( osg::Transform::ABSOLUTE_RF );
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
     osgDB::writeImageFile( *(ssImageList.at(0)), "ssImage1.jpg" );
     osgDB::writeImageFile( *(ssImageList.at(1)), "ssImage2.jpg" );
     osgDB::writeImageFile( *(ssImageList.at(2)), "ssImage3.jpg" );
     osgDB::writeImageFile( *(ssImageList.at(3)), "ssImage4.jpg" );
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
