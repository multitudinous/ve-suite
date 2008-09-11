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

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ClearNode>
#include <osg/Texture2D>
#include <osg/FrameBufferObject>

#include <osgDB/WriteFile>

#include <osgUtil/SceneView>

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

////////////////////////////////////////////////////////////////////////////////
SceneRenderToTexture::SceneRenderToTexture()
    :
    mColorTexture( 0 ),
    mDepthStencilTexture( 0 ),
    mCamera( new osg::Camera() ),
    mQuadGeode( 0 ),
    mQuadGeometry( 0 ),
    mQuadVertices( 0 ),
    mRootGroup( new osg::Group() )
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
    mColorTexture = new osg::Texture2D();
    mColorTexture->setInternalFormat( GL_RGBA16F_ARB );
    mColorTexture->setTextureSize( screenDims.first, screenDims.second );
    mColorTexture->setSourceFormat( GL_RGBA );
    mColorTexture->setSourceType( GL_FLOAT );
    mColorTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    mColorTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    mColorTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    mColorTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );

    mDepthStencilTexture = new osg::Texture2D();
    mDepthStencilTexture->setInternalFormat( GL_DEPTH24_STENCIL8_EXT );
    mDepthStencilTexture->setTextureSize( screenDims.first, screenDims.second );
    mDepthStencilTexture->setSourceFormat( GL_DEPTH_STENCIL_EXT );
    mDepthStencilTexture->setSourceType( GL_UNSIGNED_INT_24_8_EXT );
    mDepthStencilTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::NEAREST );
    mDepthStencilTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::NEAREST );
    mDepthStencilTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    mDepthStencilTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitSAQuad( std::map< std::string, double >& screenCorners )
{
    double xMin = screenCorners.find( "xmin" )->second * 3.2808399;
    double xMax = screenCorners.find( "xmax" )->second * 3.2808399;
    double yMin = screenCorners.find( "ymin" )->second * 3.2808399;
    double yMax = screenCorners.find( "ymax" )->second * 3.2808399;
    double zVal = screenCorners.find( "zval" )->second * 3.2808399;
    
    mQuadGeode = new osg::Geode();
    mQuadGeometry = new osg::Geometry();

    mQuadVertices = new osg::Vec3Array();
    mQuadVertices->resize( 4 );
    (*mQuadVertices)[ 0 ].set( xMin, -zVal, yMin );
    (*mQuadVertices)[ 1 ].set( xMax, -zVal, yMin );
    (*mQuadVertices)[ 2 ].set( xMax, -zVal, yMax );
    (*mQuadVertices)[ 3 ].set( xMin, -zVal, yMax );
    mQuadGeometry->setVertexArray( mQuadVertices.get() );
    
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0, 0 );
    (*quadTexCoords)[ 1 ].set( 1, 0 );
    (*quadTexCoords)[ 2 ].set( 1, 1 );
    (*quadTexCoords)[ 3 ].set( 0, 1 );
    mQuadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    
    mQuadGeometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );
    //mQuadGeometry->setDataVariance( osg::Object::DYNAMIC );
    //mQuadGeometry->setSupportsDisplayList(false);
    
    mQuadGeode->addDrawable( mQuadGeometry.get() );
    mQuadGeode->setCullingActive( false );
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitCamera( std::pair< int, int >& screenDims )
{
    mCamera->setReferenceFrame( osg::Camera::RELATIVE_RF );
    mCamera->setRenderOrder( osg::Camera::POST_RENDER );
    mCamera->setClearMask( 
        GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    mCamera->setClearColor( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    mCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT );
    mCamera->setViewport( 0, 0, screenDims.first, screenDims.second );
    
    //Attach a texture and use it as the render target
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 6 ) && ( OSG_VERSION_PATCH >= 0 ) )
    mCamera->attach( osg::Camera::COLOR_BUFFER, mColorTexture.get(), 0, 0, false, 4, 2 );
#else
    mCamera->attach( osg::Camera::COLOR_BUFFER, mColorTexture.get() );
#endif

    //Use an interleaved depth/stencil texture to get a depth and stencil buffer
    mCamera->attach( osg::Camera::DEPTH_BUFFER, mDepthStencilTexture.get() );
    mCamera->attach( osg::Camera::STENCIL_BUFFER, mDepthStencilTexture.get() );

    //Use renderbuffers to get a depth and stencil buffer
    //mCamera->attach( osg::Camera::DEPTH_BUFFER, GL_DEPTH_COMPONENT24 );
    //mCamera->attach( osg::Camera::STENCIL_BUFFER, GL_STENCIL_INDEX8_EXT  );
#if ( ( OSG_VERSION_MAJOR >= 2 ) && ( OSG_VERSION_MINOR >= 6 ) && ( OSG_VERSION_PATCH >= 0 ) )
    mCamera->setClearStencil( 0 );
#endif

    //This camera has a RELATIVE_RF
    //Therefore the transform is cumulative from parents transforms
    mCamera->setViewMatrix( osg::Matrix::identity() );
    mCamera->setProjectionMatrix( osg::Matrix::identity() );

    osg::ref_ptr< osg::ClearNode > clearNode = new osg::ClearNode();
    clearNode->setClearMask(
        GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );
    mCamera->addChild( clearNode.get() );

}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::InitScene()
{
    //Get state info about the screen
    std::pair< int, int > screenDims = 
        EnvironmentHandler::instance()->
        GetDisplaySettings()->GetScreenResolution();
    
    std::map< std::string, double > screenCorners = 
        EnvironmentHandler::instance()->
        GetDisplaySettings()->GetScreenCornerValues();
    
    //Create textures, camera, and SA-quad    
    InitTextures( screenDims );
    InitCamera( screenDims );
    InitSAQuad( screenCorners );

    std::string vertexSource =
    "void main() \n"
    "{ \n"
        "gl_Position = ftransform(); \n"

        "gl_TexCoord[ 0 ].st = gl_MultiTexCoord0.st; \n"
    "} \n";

    std::string fragmentSource =
    "uniform sampler2D colorMap; \n"
    "uniform sampler2D depthStencilMap; \n"

    "void main() \n"
    "{ \n"
        "vec4 color = texture2D( colorMap, gl_TexCoord[ 0 ].st ); \n"
        "vec4 depthStencil = texture2D( depthStencilMap, gl_TexCoord[ 0 ].st ); \n"

        //"depthStencil /= depthStencil.w; \n"

        "gl_FragColor = color; \n"
        //"gl_FragColor = vec4( depthStencil.x, depthStencil.y, depthStencil.z, 1.0 ); \n"
    "} \n";

    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    vertexShader->setShaderSource( vertexSource );

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    stateset->setAttribute(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setTextureAttributeAndModes(
        0, mColorTexture.get(), osg::StateAttribute::ON );
    stateset->setTextureAttributeAndModes(
        1, mDepthStencilTexture.get(), osg::StateAttribute::ON );
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    osg::ref_ptr< osg::Uniform > colorMapUniform =
        new osg::Uniform( "colorMap", 0 );
    stateset->addUniform( colorMapUniform.get() );

    osg::ref_ptr< osg::Uniform > depthStencilMapUniform =
        new osg::Uniform( "depthStencilMap", 1 );
    stateset->addUniform( depthStencilMapUniform.get() );

    mQuadGeode->setStateSet( stateset.get() );

    //Call this when changing images in the fbo
    //The setUpCamera does not run since the camera has the
    //previous RenderStage as a cached object for efficiency issues.
    //If you change the attachment, you should set the camera cache to NULL,
    //then when the rendering occurs, a new RenderStage will be created
    //for the camera and the runCameraSetup will be called again.
    //mCamera->setRenderingCache( NULL );
}
////////////////////////////////////////////////////////////////////////////////
/*void SceneRenderToTexture::LatePreFrameUpdate()
{
    ;
}*/
////////////////////////////////////////////////////////////////////////////////
osg::Camera* const SceneRenderToTexture::GetCamera() const
{
    return mCamera.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* const SceneRenderToTexture::GetGroup() const
{
    return mRootGroup.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* const SceneRenderToTexture::GetQuad() const
{
    return mQuadGeode.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* const SceneRenderToTexture::GetTexture() const
{
    return mColorTexture.get();
}
////////////////////////////////////////////////////////////////////////////////
void SceneRenderToTexture::WriteImageFileForWeb(
    osg::Group* root, osgUtil::SceneView* sv, std::string& filename )
{
    /* while(runWebImageSaveThread)
     {
     vpr::System::msleep( 500 );  // half-second delay
     if(readyToWriteWebImage)
     {
     readyToWriteWebImage=false;
     writingWebImageNow = true;
     //let's try saving the image with Corona
     corona::Image* frameCap=corona::CreateImage(webImageWidth, webImageHeight, corona::PF_R8G8B8, (void*)webImagePixelArray);
     frameCap=corona::FlipImage(frameCap, corona::CA_X);
     if(!corona::SaveImage("../../public_html/PowerPlant/VE/dump.png", corona::FF_PNG, frameCap))
     std::cout << "error saving image!" << std::endl;
     else 
     std::cout << "Image saved successfully.!" << std::endl;
     delete frameCap;
     delete [] webImagePixelArray;                             //delete our array
     std::cout << "All done!" << std::endl;
     writingWebImageNow = false;
     }
     }*/
    
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
    oldcamera->getProjectionMatrixAsFrustum( frustum[0], frustum[1],
                                            frustum[2], frustum[3], frustum[4], frustum[5] );
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
