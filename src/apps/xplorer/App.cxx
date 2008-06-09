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
#include "App.h"
#include "VjObsWrapper.h"

#include <ves/xplorer/TextureBasedVizHandler.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/environment/cfdQuatCamHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/Debug.h>

#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/network/cfdExecutive.h>

// Scene graph dependant headers
#include <osg/Group>
#include <osg/FrameStamp>
#include <osg/MatrixTransform>
#include <osg/Matrix>
#include <osg/Referenced>
#include <osg/Light>
#include <osg/LightSource>
#include <osg/CameraNode>
#include <osg/Camera>
#include <osg/Image>
#include <osg/TextureRectangle>
#include <osg/Texture2D>

#include <osgDB/WriteFile>

#include <osgUtil/SceneView>
#include <osgUtil/UpdateVisitor>
#include <osgUtil/Statistics>

#include <gmtl/Generate.h>
#include <gmtl/Coord.h>

#include <ves/xplorer/volume/cfdPBufferManager.h>
using namespace ves::xplorer::volume;

/// C/C++ libraries
#include <iostream>

#include <vrj/Kernel/Kernel.h>
#include <vrj/Draw/OGL/GlDrawManager.h>
#include <vpr/Perf/ProfileManager.h>
#include <vpr/System.h>

using namespace ves::xplorer;
using namespace ves::xplorer::util;
using namespace ves::open::xml;
using namespace ves::xplorer::network;


////////////////////////////////////////////////////////////////////////////////
App::App( int argc, char* argv[] )
#if __VJ_version >= 2003000
        :
        vrj::osg::App( vrj::Kernel::instance() ),
#else
        :
        vrj::OsgApp( vrj::Kernel::instance() ),
#endif
        readyToWriteWebImage( false ),
        writingWebImageNow( false ),
        captureNextFrameForWeb( false ),
        isCluster( false )
{
#ifdef _OSG
    osg::Referenced::setThreadSafeReferenceCounting( true );
    osg::DisplaySettings::instance()->setMaxNumberOfGraphicsContexts( 20 );
    _frameStamp = new osg::FrameStamp;
    mUpdateVisitor = new osgUtil::UpdateVisitor();
    _frameStamp->setReferenceTime( 0.0 );
    _frameStamp->setFrameNumber( 0 );
    svUpdate = false;

    light_0 = new osg::Light;
    light_source_0 = new osg::LightSource;
    light_model_0 = new osg::LightModel;

    light_0->setLightNum( 0 );
    light_0->setAmbient( osg::Vec4d( 0.36862f, 0.36842f, 0.36842f, 1.0f ) );
    light_0->setDiffuse( osg::Vec4d( 0.88627f, 0.88500f, 0.88500f, 1.0f ) );
    light_0->setSpecular( osg::Vec4d( 0.49019f, 0.48872f, 0.48872f, 1.0f ) );
    light_0->setPosition( osg::Vec4d( 0.0f, -10000.0f, 10000.0f, 0.0f ) );
    light_0->setDirection( osg::Vec3d( -1, 1, -1 ) );

    light_source_0->setLight( light_0.get() );
    light_source_0->setLocalStateSetModes( osg::StateAttribute::ON );

    light_model_0->setAmbientIntensity( osg::Vec4( 0.1f, 0.1f, 0.1f, 1.0f ) );
    // get correct specular lighting across pipes
    // see http://www.ds.arch.tue.nl/General/Staff/Joran/osg/osg_specular_problem.htm
    light_model_0->setLocalViewer( true );

    _tbvHandler = 0;
#ifdef _PBUFFER
    _pbuffer = 0;
#endif
    _frameNumber = 0;
#endif
    this->argc = argc;
    this->argv = argv;
}
////////////////////////////////////////////////////////////////////////////////
void App::exit()
{
    //Profiling guard used by vrjuggler
    VPR_PROFILE_RESULTS();
    std::cout << "|\tApp is now exiting." << std::endl;
    ves::xplorer::scenegraph::SceneManager::instance()->Shutdown();
    ves::xplorer::network::cfdExecutive::instance()->UnRegisterExecutive();
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _OSG
osg::Group* App::getScene()
#endif
{
    //osgDB::writeNodeFile(*this->_sceneManager->GetRootNode()->GetRawNode(),
    //   "C:/test.osg");
#ifdef _OSG
    return ( osg::Group* )ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();
#endif
}

#ifdef _OSG
////////////////////////////////////////////////////////////////////////////////
void App::contextInit()
{
    //vrj::OsgApp::contextInit();

    const unsigned int unique_context_id =
        vrj::GlDrawManager::instance()->getCurrentContext();

    // --- Create new context specific scene viewer -- //
    osg::ref_ptr<osgUtil::SceneView> new_sv( new osgUtil::SceneView );
    this->configSceneView( new_sv.get() );          // Configure the new viewer
    // hard code the LOD setting to be something high for the time being
    new_sv->setLODScale( 0.01f );
    // set the unique id for this particular context
    new_sv->getState()->setContextID( unique_context_id );
    // Add the tree to the scene viewer and set properties
    {
        vpr::Guard<vpr::Mutex> sv_guard( mValueLock );
        new_sv->setSceneData( getScene() );
    }

    ( *sceneViewer ) = new_sv;

#ifdef _PBUFFER
    if( !_pbuffer )
    {
        _pbuffer = new cfdPBufferManager();
        _pbuffer->isSupported();
    }

    _tbvHandler->SetPBuffer( _pbuffer );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void App::contextClose()
{
#ifdef _PBUFFER
    if( _pbuffer )
    {
        delete _pbuffer;
        _pbuffer = 0;
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _PBUFFER
cfdPBufferManager* App::GetPBuffer()
{
    return _pbuffer;
}
#endif
////////////////////////////////////////////////////////////////////////////////
/*osgUtil::SceneView::Options App::getSceneViewDefaults()
{
    return osgUtil::SceneView::COMPILE_GLOBJECTS_AT_INIT;
}*/
////////////////////////////////////////////////////////////////////////////////
void App::configSceneView( osgUtil::SceneView* newSceneViewer )
{
    newSceneViewer->setDefaults( osgUtil::SceneView::COMPILE_GLOBJECTS_AT_INIT );

    // Set the timing information in the scene view. This has to be done
    // only once per osgUtil::SceneView instance and should be done before
    // calling osgUtil::SceneView::init().
    newSceneViewer->setFrameStamp( _frameStamp.get() );

    newSceneViewer->init();
    newSceneViewer->setClearColor( osg::Vec4( 0.0f, 0.0f, 0.0f, 0.0f ) );

    {
        vpr::Guard<vpr::Mutex> val_guard( mValueLock );
        // Needed for stereo to work.
        newSceneViewer->setDrawBufferValue( GL_NONE );

        newSceneViewer->getGlobalStateSet()->setAssociatedModes(
            light_0.get(), osg::StateAttribute::ON );

        newSceneViewer->getGlobalStateSet()->setMode(
            GL_LIGHTING, osg::StateAttribute::ON );

        newSceneViewer->getGlobalStateSet()->setAttributeAndModes(
            light_model_0.get(), osg::StateAttribute::ON );

        newSceneViewer->setSmallFeatureCullingPixelSize( 10 );
    }

    ///With this code in culling culs the near and far planes. I believe
    ///we discovered this awhile ago but removed the comments about it.
    ///Please see cullvisitor (osg/CullSettings)
    // for the possible settings for this function.
    //This defaults to setting the near and far plane based on the
    //bounding volume.
    //newSceneViewer->setComputeNearFarMode(
    //    osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
    newSceneViewer->setComputeNearFarMode(
        osgUtil::CullVisitor::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::bufferPreDraw()
{
    ;
}
#endif //_OSG
////////////////////////////////////////////////////////////////////////////////
void App::SetWrapper( VjObsWrapper* input )
{
    m_vjobsWrapper = input;
}
////////////////////////////////////////////////////////////////////////////////
void App::initScene( void )
{
    vprDEBUG( vesDBG, 0 ) << "App::initScene" << std::endl << vprDEBUG_FLUSH;
    //Initialize all the XML objects
    XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new XMLCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new shader::ShaderCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Model", new model::ModelCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new cad::CADCreator() );

    std::cout << std::endl;
    std::cout << "| ***************************************************************** |" << std::endl;
    m_vjobsWrapper->InitCluster();
    // define the rootNode, worldDCS, and lighting
    ves::xplorer::scenegraph::SceneManager::instance()->InitScene();

    this->getScene()->addChild( light_source_0.get() );

#ifdef _OSG
    ves::xplorer::scenegraph::SceneManager::instance()->ViewLogo( true );
#endif

    // modelHandler stores the arrow and holds all data and geometry
    ModelHandler::instance()->SetXMLCommand( m_vjobsWrapper->GetXMLCommand() );
    ModelHandler::instance()->InitScene();

    // navigation and cursor
    EnvironmentHandler::instance()->Initialize();
    for( int i = 1;i < argc;++i )
    {
        if (( std::string( argv[i] ) == std::string( "-VESDesktop" ) ) && ( argc >= i + 2 ) )
        {
            EnvironmentHandler::instance()->
            SetDesktopSize( atoi( argv[i+1] ), atoi( argv[i+2] ) );
        }
        else if( std::string( argv[i] ) == std::string( "-VESCluster" ) )
        {
            isCluster = true;
        }
    }
    EnvironmentHandler::instance()->InitScene();
    cfdQuatCamHandler::instance()->SetMasterNode( m_vjobsWrapper->IsMaster() );

    // create steady state visualization objects
    SteadyStateVizHandler::instance()->Initialize( std::string() );
    SteadyStateVizHandler::instance()->InitScene();

    //create the volume viz handler
#ifdef _OSG
    _start_tick = _timer.tick();
    _tbvHandler = ves::xplorer::TextureBasedVizHandler::instance();
    _tbvHandler->SetMasterNode( m_vjobsWrapper->IsMaster() );
#endif

    std::cout << "|  2. Initializing.................................... cfdExecutive |" << std::endl;
    cfdExecutive::instance()->Initialize( m_vjobsWrapper->naming_context, m_vjobsWrapper->child_poa );

    // This may need to be fixed
    this->m_vjobsWrapper->GetCfdStateVariables();

    //Setup near and far plane
    float nearPlane;
    float farPlane;
    vrj::Projection::getNearFar( nearPlane, farPlane );
    vrj::Projection::setNearFar( nearPlane, farPlane + 100000 );
}
////////////////////////////////////////////////////////////////////////////////
void App::preFrame( void )
{
    VPR_PROFILE_GUARD_HISTORY( "App::preFrame", 20 );
    vprDEBUG( vesDBG, 3 ) << "|App::preFrame" << std::endl << vprDEBUG_FLUSH;
    //Check and see if the ord has any work to do
    m_vjobsWrapper->CheckORBWorkLoad();
    //Sets the worldDCS before it is synced
    EnvironmentHandler::instance()->PreFrameUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void App::latePreFrame( void )
{
    VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame", 20 );
    static long lastFrame = 0;
    //Used for framerate calculation as integers only
    static float lastTime = 0.0f;

    vprDEBUG( vesDBG, 3 ) << "|App::latePreFrame" << std::endl << vprDEBUG_FLUSH;
    //The calls below are order dependent so do not move them around
    //call the parent method
    m_vjobsWrapper->GetUpdateClusterStateVariables();
    //This should be called after the update so that
    //all the singletons below get the updated command
    m_vjobsWrapper->PreFrameUpdate();
    //Exit - must be called AFTER m_vjobsWrapper->PreFrameUpdate();
    if( m_vjobsWrapper->GetXMLCommand()->GetCommandName() == "EXIT_XPLORER" )
    {
        std::cout << "|\tShutting down xplorer." << std::endl;
        VPR_PROFILE_RESULTS();
        // exit App was selected
        vrj::Kernel::instance()->stop(); // Stopping kernel
    }

    float current_time = this->m_vjobsWrapper->GetSetAppTime( -1 );
#ifdef _OSG
    //This is order dependent
    //don't move above function call
    _frameStamp->setFrameNumber( _frameNumber );
    _frameStamp->setReferenceTime( current_time );
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
    _frameStamp->setSimulationTime( current_time );
#endif
    //This is a frame rate calculation
    float deltaTime = current_time - lastTime;
    ves::xplorer::scenegraph::PhysicsSimulator::instance()->UpdatePhysics( deltaTime );
    if( deltaTime >= 1.0f )
    {
        float framerate;
        framerate = _frameNumber - lastFrame;
        ves::xplorer::EnvironmentHandler::instance()->SetFrameRate( framerate );

        lastTime = current_time;
        lastFrame = _frameNumber;
    }
    
    if( ( vpr::Debug::instance()->isDebugEnabled() ) )// && ( 3 <= vpr::Debug::instance()->getLevel() ) )
    {
        if( ( _frameNumber % 500 ) == 0.0f )
        {
            vprDEBUG( vesDBG, 3 ) << " App::latePreFrame Profiling data for frame "
                << _frameNumber << " and time " << current_time << std::endl << vprDEBUG_FLUSH;
            VPR_PROFILE_RESULTS();
        }
        
        if( 3 >= vpr::Debug::instance()->getLevel() )
        {
            osgUtil::StatsVisitor stats;
            getScene()->accept( stats );
            std::ostringstream statsStream;
            stats.print( statsStream );
            vprDEBUG( vesDBG, 3 ) << statsStream.str() << std::endl 
                << vprDEBUG_FLUSH;
        }        
    }    
#endif

    ves::xplorer::scenegraph::SceneManager::instance()->PreFrameUpdate();
    ///////////////////////
    ModelHandler::instance()->PreFrameUpdate();
    ///////////////////////
    EnvironmentHandler::instance()->LatePreFrameUpdate();
    ///////////////////////
    SteadyStateVizHandler::instance()->PreFrameUpdate();

    _tbvHandler->SetCurrentTime( this->m_vjobsWrapper->GetSetAppTime( -1 ) );
    _tbvHandler->PreFrameUpdate();
    ///////////////////////
    cfdExecutive::instance()->PreFrameUpdate();

#ifdef _OSG
    //profile the update call
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame update", 20 );
        this->update();
    }
#endif
    ///Increment framenumber now that we are done using it everywhere
    _frameNumber += 1;

    if( m_vjobsWrapper->GetXMLCommand()->GetCommandName() == "SCREEN_SHOT" )
    {
        captureNextFrameForWeb = true;
        m_vjobsWrapper->GetXMLCommand()->GetDataValuePair( "Filename" )->GetData( m_filename );
    }
    vprDEBUG( vesDBG, 3 ) << "|App::End latePreFrame" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::intraFrame()
{
    vprDEBUG( vesDBG, 3 ) << "|intraFrame" << std::endl << vprDEBUG_FLUSH;
    // Do nothing here
    // Usually slows things down
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _OSG
void App::contextPostDraw()
{
    VPR_PROFILE_GUARD_HISTORY( "App::contextPostDraw", 20 );
    _tbvHandler->PingPongTextures();
    //here for testing...
    glFinish();
}
#endif//_OSG
////////////////////////////////////////////////////////////////////////////////
void App::postFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::postFrame", 20 );
    vprDEBUG( vesDBG, 3 ) << "|postFrame" << std::endl << vprDEBUG_FLUSH;

#ifdef _OSG
    //svUpdate = false;
    //cfdEnvironmentHandler::instance()->ResetBackgroundColorUpdateFlag();
    time_since_start = _timer.delta_s( _start_tick, _timer.tick() );
#endif  //_OSG


#ifdef _OSG
    this->m_vjobsWrapper->GetSetAppTime( time_since_start );
    EnvironmentHandler::instance()->PostFrameUpdate();
    //this->m_vjobsWrapper->GetSetFrameNumber( _frameNumber++ );

    ///update the transient frame number on the master
    _tbvHandler->UpdateTransientFrame();
#endif   //_OSG
    cfdExecutive::instance()->PostFrameUpdate();

    this->m_vjobsWrapper->GetCfdStateVariables();
    vprDEBUG( vesDBG, 3 ) << "|End postFrame" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::writeImageFileForWeb()
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

    vpr::Guard<vpr::Mutex> val_guard( mValueLock );
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
    osg::ref_ptr<osg::Node> subgraph = new osg::Group( *getScene() );
    std::vector< osg::ref_ptr< osg::Camera > > rttCameraList;

    ///create the screen shot root
    osg::ref_ptr< osg::Group > screenShotRoot = new osg::Group;

    ///create the list of RTT's
    std::vector< osg::ref_ptr< osg::Texture2D > >rttList;
    
    osg::ref_ptr<osgUtil::SceneView> sv;
    sv = ( *sceneViewer );  // Get context specific scene viewer
    /* this doesn't seem like the right place for this but
      sceneview isn't exposed anywhere else
     sv->setLODScale( EnvironmentHandler::instance()->GetGlobalLODScale() );
     */
    osg::ref_ptr<osg::CameraNode> oldcamera = sv->getCamera();
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
        rttCameraList.back()->setRenderOrder( osg::CameraNode::PRE_RENDER );

        // tell the camera to use OpenGL frame buffer object where supported.
        rttCameraList.back()->setRenderTargetImplementation(
            osg::CameraNode::FRAME_BUFFER_OBJECT );
        // add subgraph to render
        rttCameraList.back()->addChild( subgraph.get() );

        // set viewport
        rttCameraList.back()->setViewport( 0, 0, w*2, h*2 );

        ///Attach the camera to the image
        rttCameraList.back()->attach( osg::CameraNode::COLOR_BUFFER,
                                      rttList.back().get() );
        screenShotRoot->addChild( rttCameraList.back().get() );
    }

    std::vector< osg::ref_ptr<osg::CameraNode> >::iterator activeCamera;
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
    getScene()->addChild( screenShotRoot.get() );
    
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
                                             osg::CameraNode::FRAME_BUFFER_OBJECT );

        fullScreenQuadCameraList.back()->setViewport( 0, 0, w, h );
        fullScreenQuadCameraList.back()->setViewMatrix( osg::Matrix::identity() );
        fullScreenQuadCameraList.back()->setProjectionMatrix( osg::Matrix::identity() );

        ///Attach the camera to the output image
        fullScreenQuadCameraList.back()->attach( osg::CameraNode::COLOR_BUFFER,
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
    getScene()->removeChild( screenShotRoot.get() );

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
    osgDB::writeImageFile( *( shot.get() ), m_filename );
}
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::contextPreDraw( void )
{
    VPR_PROFILE_GUARD_HISTORY( "App::contextPreDraw", 20 );
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::draw()
{
    VPR_PROFILE_GUARD_HISTORY( "App::draw", 20 );
    glClear( GL_DEPTH_BUFFER_BIT );

    // Users have reported problems with OpenGL reporting stack underflow
    // problems when the texture attribute bit is pushed here, so we push all
    // attributes *except* GL_TEXTURE_BIT.
    glPushAttrib( GL_ALL_ATTRIB_BITS & ~GL_TEXTURE_BIT );
    glPushAttrib( GL_TRANSFORM_BIT );
    glPushAttrib( GL_VIEWPORT_BIT );

    glMatrixMode( GL_MODELVIEW );
    glPushMatrix();

    glMatrixMode( GL_PROJECTION );
    glPushMatrix();

    osg::ref_ptr<osgUtil::SceneView> sv;
    sv = ( *sceneViewer );  // Get context specific scene viewer
    vprASSERT( sv.get() != NULL );

    // The OpenGL Draw Manager that we are rendering for.
    //Get the view matrix and the frustrum form the draw manager
#if __VJ_version >= 2003000
    vrj::opengl::DrawManager* gl_manager =
        dynamic_cast<vrj::opengl::DrawManager*>( this->getDrawManager() );
    vprASSERT( gl_manager != NULL );
    vrj::opengl::UserData* user_data = gl_manager->currentUserData();
#else
    vrj::GlDrawManager* gl_manager =
        dynamic_cast<vrj::GlDrawManager*>( this->getDrawManager() );
    vprASSERT( gl_manager != NULL );
    vrj::GlUserData* user_data = gl_manager->currentUserData();
#endif

    // Set the up the viewport (since OSG clears it out)
    float vp_ox, vp_oy, vp_sx, vp_sy;   // The float vrj sizes of the view ports
    int w_ox, w_oy, w_width, w_height;  // Origin and size of the window
    user_data->getViewport()->getOriginAndSize( vp_ox, vp_oy, vp_sx, vp_sy );
    user_data->getGlWindow()->getOriginSize( w_ox, w_oy, w_width, w_height );

    // compute unsigned versions of the viewport info (for passing to glViewport)
    const unsigned int ll_x =
        static_cast<unsigned int>( vp_ox * static_cast<float>( w_width ) );
    const unsigned int ll_y =
        static_cast<unsigned int>( vp_oy * static_cast<float>( w_height ) );
    const unsigned int x_size =
        static_cast<unsigned int>( vp_sx * static_cast<float>( w_width ) );
    const unsigned int y_size =
        static_cast<unsigned int>( vp_sy * static_cast<float>( w_height ) );

    sv->setViewport( ll_x, ll_y, x_size, y_size );

    //Get the frustrum
#if __VJ_version >= 2003000
    vrj::ProjectionPtr project = user_data->getProjection();
#else
    vrj::Projection* project = user_data->getProjection();
#endif
    vrj::Frustum frustum = project->getFrustum();
    sv->setProjectionMatrixAsFrustum( frustum[vrj::Frustum::VJ_LEFT],
                                      frustum[vrj::Frustum::VJ_RIGHT],
                                      frustum[vrj::Frustum::VJ_BOTTOM],
                                      frustum[vrj::Frustum::VJ_TOP],
                                      frustum[vrj::Frustum::VJ_NEAR],
                                      frustum[vrj::Frustum::VJ_FAR] );

    //Allow trackball to grab frustum values to calculate FOVy
    EnvironmentHandler::instance()->SetFrustumValues( frustum[vrj::Frustum::VJ_LEFT],
                                                      frustum[vrj::Frustum::VJ_RIGHT],
                                                      frustum[vrj::Frustum::VJ_TOP],
                                                      frustum[vrj::Frustum::VJ_BOTTOM],
                                                      frustum[vrj::Frustum::VJ_NEAR],
                                                      frustum[vrj::Frustum::VJ_FAR] );

    // Copy the view matrix
    gmtl::Vec3f x_axis( 1.0f, 0.0f, 0.0f );
    gmtl::Matrix44f _vjMatrixLeft( project->getViewMatrix() );
    gmtl::postMult( _vjMatrixLeft, gmtl::makeRot<gmtl::Matrix44f>(
                        gmtl::AxisAnglef( gmtl::Math::deg2Rad( -90.0f ), x_axis ) ) );
    //copy the matrix
    osg::ref_ptr<osg::RefMatrix> osg_proj_xform_mat = new osg::RefMatrix;
    osg_proj_xform_mat->set( _vjMatrixLeft.mData );
    sv->setViewMatrix( *( osg_proj_xform_mat.get() ) );

    //Draw the scene
    // NOTE: It is not safe to call osgUtil::SceneView::update() here; it
    // should only be called by a single thread. The equivalent of calling
    // osgUtil::SceneView::update() is in vrj::OsgApp::update().
    //profile the cull call
    {
        VPR_PROFILE_GUARD_HISTORY( "App::draw sv->cull", 20 );
        sv->cull();
    }
    //profile the draw call
    {
        VPR_PROFILE_GUARD_HISTORY( "App::draw sv->draw", 20 );
        sv->draw();
    }
    ///Screen capture code
    if( captureNextFrameForWeb )
    {
        //gl_manager->currentUserData()->getViewport()->isSimulator();
        //gl_manager->currentUserData()->getGlWindow()->getId();
        writeImageFileForWeb();
        captureNextFrameForWeb = false;
    }

    glMatrixMode( GL_PROJECTION );
    glPopMatrix();

    glMatrixMode( GL_MODELVIEW );
    glPopMatrix();

    glPopAttrib();
    glPopAttrib();
    glPopAttrib();

    //Here for testing purposes
    glFlush();
}
////////////////////////////////////////////////////////////////////////////////
void App::update( void )
{
    // Update the frame stamp with information from this frame
    //frameStamp->setFrameNumber( getFrameNumber() );
    //frameStamp->setReferenceTime( getFrameTime().secd() );

    // Set up the time and frame number so time dependant things (animations, particle system)
    // function correctly
    mUpdateVisitor->setTraversalNumber( _frameNumber );
    mUpdateVisitor->setFrameStamp( _frameStamp.get() );

    // update the scene by traversing it with the the update visitor which will
    // call all node update callbacks and animations. This is equivalent to calling
    // SceneView::update
    getScene()->accept( *mUpdateVisitor.get() );
    // now force a recompute of the bounding volume while we are still in
    // the read/write app phase, this should prevent the need to recompute
    // the bounding volumes from within the cull traversal which may be
    // multi-threaded.
    getScene()->getBound();
}
