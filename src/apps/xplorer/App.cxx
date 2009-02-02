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
#include "App.h"
#include "VjObsWrapper.h"
#include "SceneRenderToTexture.h"

#include <ves/xplorer/TextureBasedVizHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/util/fileIO.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/environment/cfdQuatCamHandler.h>
#include <ves/xplorer/network/cfdExecutive.h>
#include <ves/xplorer/volume/cfdPBufferManager.h>

#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/cad/CADCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/model/ModelCreator.h>
#include <ves/open/xml/model/Model.h>

// --- OSG Includes --- //
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
#include <osg/DeleteHandler>

#include <osgDB/WriteFile>

#include <osgUtil/SceneView>
#include <osgUtil/UpdateVisitor>
#include <osgUtil/Statistics>

// --- VR Juggler Includes --- //
#include <gmtl/Generate.h>
#include <gmtl/Coord.h>
#include <gmtl/Misc/MatrixConvert.h>
#include <gmtl/Xforms.h>

#include <vrj/Kernel/Kernel.h>
#include <vrj/Draw/OGL/GlDrawManager.h>
#include <vrj/Display/DisplayManager.h>

#include <vpr/Perf/ProfileManager.h>
#include <vpr/System.h>

#include <jccl/RTRC/ConfigManager.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer;
using namespace ves::xplorer::util;
using namespace ves::xplorer::volume;
using namespace ves::open::xml;
using namespace ves::xplorer::network;

////////////////////////////////////////////////////////////////////////////////
App::App( int argc, char* argv[] )
#if __VJ_version >= 2003000
    : vrj::osg::App( vrj::Kernel::instance() ),
#else
    : vrj::OsgApp( vrj::Kernel::instance() ),
#endif
    readyToWriteWebImage( false ),
    writingWebImageNow( false ),
    captureNextFrameForWeb( false ),
    isCluster( false ),
    mLastFrame( 0 ),
    mLastTime( 0 ),
    mProfileCounter( 0 ),
    mRTT( true )
{
    osg::Referenced::setThreadSafeReferenceCounting( true );
    osg::DisplaySettings::instance()->setMaxNumberOfGraphicsContexts( 20 );
    mFrameStamp = new osg::FrameStamp;
    mUpdateVisitor = new osgUtil::UpdateVisitor();
    mFrameStamp->setReferenceTime( 0.0 );
    mFrameStamp->setFrameNumber( 0 );
    svUpdate = false;

    light_0 = new osg::Light;
    light_source_0 = new osg::LightSource;
    light_model_0 = new osg::LightModel;

    light_0->setLightNum( 0 );
    light_0->setAmbient( osg::Vec4d( 0.36862, 0.36842, 0.36842, 1.0 ) );
    light_0->setDiffuse( osg::Vec4d( 0.88627, 0.88500, 0.88500, 1.0 ) );
    light_0->setSpecular( osg::Vec4d( 0.49019, 0.48872, 0.48872, 1.0 ) );
    //We are in openGL space
    light_0->setPosition( osg::Vec4d( 0.0, -10000.0, 10000.0, 0.0 ) );
    light_0->setDirection( osg::Vec3d( 0.0, 1.0, -1.0 ) );

    light_source_0->setLight( light_0.get() );
    light_source_0->setLocalStateSetModes( osg::StateAttribute::ON );
    // See the opengl docs on what the difference 
    // is between ABSOLUTE and RELATIVE
    light_source_0->setReferenceFrame( osg::LightSource::RELATIVE_RF );

    light_model_0->setAmbientIntensity( osg::Vec4( 0.1, 0.1, 0.1, 1.0 ) );
    // get correct specular lighting across pipes
    // see http://www.ds.arch.tue.nl/General/Staff/Joran/osg/osg_specular_problem.htm
    light_model_0->setLocalViewer( true );

    _tbvHandler = 0;
#ifdef _PBUFFER
    _pbuffer = 0;
#endif
    _frameNumber = 0;
    this->argc = argc;
    this->argv = argv;
    
    mSceneRenderToTexture = 
        SceneRenderToTexturePtr( new SceneRenderToTexture() );
    
    gmtl::Vec3f x_axis( 1.0f, 0.0f, 0.0f );
    mZUp = gmtl::makeRot< gmtl::Matrix44f >( 
        gmtl::AxisAnglef( gmtl::Math::deg2Rad( -90.0f ), x_axis ) );
}
////////////////////////////////////////////////////////////////////////////////
App::~App()
{
    ;
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
osg::Group* App::getScene()
{
    //osgDB::writeNodeFile(*this->_sceneManager->GetRootNode()->GetRawNode(),
    //   "C:/test.osg");
    return ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();
}
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
        new_sv->getCamera()->setName( "SV Camera" );
        if( !mRTT )
        {
            new_sv->setSceneData( getScene() );
        }
        else
        {
            *mAlreadyRendered = false;
        }
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
    newSceneViewer->setFrameStamp( mFrameStamp.get() );

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
////////////////////////////////////////////////////////////////////////////////
void App::SetWrapper( VjObsWrapper* input )
{
    m_vjobsWrapper = input;
}
////////////////////////////////////////////////////////////////////////////////
void App::initScene()
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
    ves::xplorer::scenegraph::SceneManager::instance()->SetRootNode(
            mSceneRenderToTexture->GetGroup() );
    ves::xplorer::scenegraph::SceneManager::instance()->InitScene();
    ves::xplorer::scenegraph::SceneManager::instance()->ViewLogo( true );
    ves::xplorer::scenegraph::SceneManager::instance()->
        SetFrameStamp( mFrameStamp.get() );

    getScene()->addChild( light_source_0.get() );
    getScene()->getOrCreateStateSet()->setAttributeAndModes( light_model_0.get(), osg::StateAttribute::ON );

    // modelHandler stores the arrow and holds all data and geometry
    ModelHandler::instance()->SetXMLCommand( m_vjobsWrapper->GetXMLCommand() );
    ModelHandler::instance()->InitScene();

    // navigation and cursor
    EnvironmentHandler::instance()->Initialize();
    for( int i = 1;i < argc;++i )
    {
        if( ( std::string( argv[i] ) == std::string( "-VESDesktop" ) ) && 
            ( argc >= i + 2 ) )
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
}
////////////////////////////////////////////////////////////////////////////////
void App::preFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::preFrame", 20 );
    vprDEBUG( vesDBG, 3 ) << "|App::preFrame" << std::endl << vprDEBUG_FLUSH;
    ///////////////////////
    {
        //Check and see if the ord has any work to do
        VPR_PROFILE_GUARD_HISTORY( "App::preFrame CheckORBWorkLoad", 20 );
        m_vjobsWrapper->CheckORBWorkLoad();
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::preFrame EnvironmentHandler", 20 );
        //Sets the worldDCS before it is synced
        EnvironmentHandler::instance()->PreFrameUpdate();
    }
    ///////////////////////
}
////////////////////////////////////////////////////////////////////////////////
void App::latePreFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame", 20 );
    const std::string tempCommandName = 
        m_vjobsWrapper->GetXMLCommand()->GetCommandName();
    vprDEBUG( vesDBG, 3 ) << "|App::latePreFrame" << std::endl << vprDEBUG_FLUSH;
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame m_vjobsWrapper", 20 );
        //The calls below are order dependent so do not move them around
        //call the parent method
        m_vjobsWrapper->GetUpdateClusterStateVariables();
        //This should be called after the update so that
        //all the singletons below get the updated command
        m_vjobsWrapper->PreFrameUpdate();
    }
    //Exit - must be called AFTER m_vjobsWrapper->PreFrameUpdate();
    if( tempCommandName == "EXIT_XPLORER" )
    {
        std::cout << "|\tShutting down xplorer." << std::endl;
        VPR_PROFILE_RESULTS();
        // exit App was selected
        vrj::Kernel::instance()->stop(); // Stopping kernel
    }
    else if( !tempCommandName.compare( "SCREEN_SHOT" ) )
    {
        captureNextFrameForWeb = true;
        m_vjobsWrapper->GetXMLCommand()->
            GetDataValuePair( "Filename" )->GetData( m_filename );
    }
    
    float deltaTime = 0;
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame Framerate Calculations", 20 );
        float current_time = this->m_vjobsWrapper->GetSetAppTime( -1 );
        //This is order dependent
        //don't move above function call
        mFrameStamp->setFrameNumber( _frameNumber );
        mFrameStamp->setReferenceTime( current_time );
#if ((OSG_VERSION_MAJOR>=1) && (OSG_VERSION_MINOR>2) || (OSG_VERSION_MAJOR>=2))
        mFrameStamp->setSimulationTime( current_time );
#endif
        mFrameDT = current_time - mLastFrameTime;
        mLastFrameTime = current_time;
        //This is a frame rate calculation
        deltaTime = current_time - mLastTime;
        if( deltaTime > 1 )
        {
            float framerate;
            framerate = _frameNumber - mLastFrame;
            ves::xplorer::EnvironmentHandler::instance()->SetFrameRate( framerate );

            mLastTime = current_time;
            mLastFrame = _frameNumber;
        }

        if( vpr::Debug::instance()->isDebugEnabled() )
        {
            if( mProfileCounter == 500 )
            {
                vprDEBUG( vesDBG, 3 ) << " App::latePreFrame Profiling data for frame "
                    << _frameNumber << " and time " << current_time << std::endl << vprDEBUG_FLUSH;
                VPR_PROFILE_RESULTS();
                mProfileCounter = 0;
            }

            if( 2 < vpr::Debug::instance()->getLevel() )
            {
                mStatsStream.str( "" );
                osgUtil::StatsVisitor stats;
                getScene()->accept( stats );
                stats.print( mStatsStream );
                vprDEBUG( vesDBG, 3 ) << mStatsStream.str() << std::endl 
                    << vprDEBUG_FLUSH;
            }        
        }
        
        ///This came from OSG/src/osgViewer/Viewer.cpp line 541
        ///I am not sure what it does but it seems important
        if( osg::Referenced::getDeleteHandler() )
        {
            osg::Referenced::getDeleteHandler()->flush();
            osg::Referenced::getDeleteHandler()->
                setFrameNumber( mFrameStamp->getFrameNumber() );
        }        
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame PhysicsSimulator", 20 );
        ves::xplorer::scenegraph::PhysicsSimulator::instance()->UpdatePhysics( mFrameDT );
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame SceneManager", 20 );
        ves::xplorer::scenegraph::SceneManager::instance()->PreFrameUpdate();
        gmtl::Matrix44d tempNavMatrix = 
            ves::xplorer::scenegraph::SceneManager::instance()->
                GetInvertedWorldDCS();
        gmtl::Vec4d tempVec( 0.0, -10000.0, 10000.0, 0.0 );
        tempVec = tempNavMatrix * tempVec;
        light_0->setPosition( 
            osg::Vec4d( tempVec[ 0 ], tempVec[ 1 ], tempVec[ 2 ], 0 ) );
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame ModelHandler", 20 );
        ModelHandler::instance()->PreFrameUpdate();
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame EnvironmentHandler", 20 );
        EnvironmentHandler::instance()->LatePreFrameUpdate();
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame SteadyStateVizHandler", 20 );
        SteadyStateVizHandler::instance()->PreFrameUpdate();
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame _tbvHandler", 20 );
        _tbvHandler->SetCurrentTime( this->m_vjobsWrapper->GetSetAppTime( -1 ) );
        _tbvHandler->PreFrameUpdate();
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame cfdExecutive", 20 );
        cfdExecutive::instance()->PreFrameUpdate();
    }
    //profile the update call
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame update", 20 );
        this->update();
    }
    ///Increment framenumber now that we are done using it everywhere
    _frameNumber += 1;
    mProfileCounter += 1;
    ///Grab nav data
    mNavPosition = 
        gmtl::convertTo< float >( 
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveNavSwitchNode()->GetMat() );
    vprDEBUG( vesDBG, 3 ) << "|App::End latePreFrame" 
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::intraFrame()
{
    vprDEBUG( vesDBG, 3 ) << "|intraFrame" << std::endl << vprDEBUG_FLUSH;
    // Do nothing here
    // Usually slows things down
}
////////////////////////////////////////////////////////////////////////////////
void App::contextPostDraw()
{
    VPR_PROFILE_GUARD_HISTORY( "App::contextPostDraw", 20 );
    _tbvHandler->PingPongTextures();
}
////////////////////////////////////////////////////////////////////////////////
void App::postFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::postFrame", 20 );
    vprDEBUG( vesDBG, 3 ) << "|postFrame" << std::endl << vprDEBUG_FLUSH;

    time_since_start = _timer.delta_s( _start_tick, _timer.tick() );

    this->m_vjobsWrapper->GetSetAppTime( time_since_start );
    EnvironmentHandler::instance()->PostFrameUpdate();
    //this->m_vjobsWrapper->GetSetFrameNumber( _frameNumber++ );

    ///update the transient frame number on the master
    _tbvHandler->UpdateTransientFrame();
    cfdExecutive::instance()->PostFrameUpdate();

    this->m_vjobsWrapper->GetCfdStateVariables();
    vprDEBUG( vesDBG, 3 ) << "|End postFrame" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::contextPreDraw()
{
    //std::cout << "----------contextPreDraw-----------" << std::endl;
    VPR_PROFILE_GUARD_HISTORY( "App::contextPreDraw", 20 );
    if( mRTT )
    {
        static bool changed = false;
        if( !changed && (_frameNumber > 10) )
        {
            if( jccl::ConfigManager::instance()->isPendingStale() )
            {
                vpr::Guard<vpr::Mutex> val_guard( mValueLock );
                mSceneRenderToTexture->InitScene( (*sceneViewer)->getCamera() );
                changed = true;
            }
        }
        *mAlreadyRendered = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::draw()
{
    if( mRTT )
    {
        if( *mAlreadyRendered )
        {
            return;
        }
    }

    //std::cout << "----------Draw-----------" << std::endl;
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

    osg::ref_ptr< osgUtil::SceneView > sv;
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
        dynamic_cast< vrj::GlDrawManager* >( this->getDrawManager() );
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
        static_cast< unsigned int >( vp_ox * static_cast< float >( w_width ) );
    const unsigned int ll_y =
        static_cast< unsigned int >( vp_oy * static_cast< float >( w_height ) );
    const unsigned int x_size =
        static_cast< unsigned int >( vp_sx * static_cast< float >( w_width ) );
    const unsigned int y_size =
        static_cast< unsigned int >( vp_sy * static_cast< float >( w_height ) );

    if( mRTT )
    {
        sv->setViewport(  0, 0, w_width, w_height  );
    }
    else
    {
        sv->setViewport(  ll_x, ll_y, x_size, y_size  );
    }

    //Get the frustrum
#if __VJ_version >= 2003000
    vrj::ProjectionPtr project = user_data->getProjection();
#else
    vrj::Projection* project = user_data->getProjection();
#endif

    vrj::Frustum frustum = project->getFrustum();
    double _left( frustum[ vrj::Frustum::VJ_LEFT ] );
    double _right( frustum[ vrj::Frustum::VJ_RIGHT ] );
    double _bottom( frustum[ vrj::Frustum::VJ_BOTTOM ] );
    double _top( frustum[ vrj::Frustum::VJ_TOP ] );
    double _near( frustum[ vrj::Frustum::VJ_NEAR ] );
    double _far( frustum[ vrj::Frustum::VJ_FAR ] );
    if( mRTT )
    {
        sv->setProjectionMatrix( osg::Matrixd::identity() );
    }
    else
    {
        sv->setProjectionMatrixAsFrustum(
            _left, _right, _bottom, _top, _near, _far );
    }
                                      
    //Copy the view matrix
    //    gmtl::Vec3f x_axis( 1.0f, 0.0f, 0.0f );
    //gmtl::postMult(
    //    _vjMatrixLeft, gmtl::makeRot< gmtl::Matrix44f >(
    //        gmtl::AxisAnglef( gmtl::Math::deg2Rad( -90.0f ), x_axis ) ) );
    gmtl::Matrix44f _vjMatrixLeft( project->getViewMatrix() );
    //Transform into z-up land
    _vjMatrixLeft = _vjMatrixLeft * mZUp * mNavPosition;
    
    //Copy the matrix
    if( mRTT )
    {
        sv->setViewMatrix( osg::Matrixd::identity() );
    }
    else
    {
        osg::ref_ptr< osg::RefMatrix > osg_proj_xform_mat = new osg::RefMatrix();
        osg_proj_xform_mat->set( _vjMatrixLeft.mData );
        sv->setViewMatrix( *(osg_proj_xform_mat.get()) );
    }

    //Setup render to texture and post-processing pipeline
    if( mRTT )
    {
        VPR_PROFILE_GUARD_HISTORY( "App::draw RTT Camera", 20 );
        //mSceneRenderToTexture->UpdateRTTQuadAndViewport();
        mSceneRenderToTexture->ConfigureRTTCameras();
    }

    //Draw the scene
    // NOTE: It is not safe to call osgUtil::SceneView::update() here; it
    // should only be called by a single thread. The equivalent of calling
    // osgUtil::SceneView::update() is in vrj::OsgApp::update().
    //profile the cull call
    {
        //vpr::Guard<vpr::Mutex> sv_guard( mValueLock );
        VPR_PROFILE_GUARD_HISTORY( "App::draw sv->cull", 20 );
        //Not sure if it should be used - came from osgViewer::Renderer::cull/draw
        //sv->inheritCullSettings( *(sv->getCamera()) );
        sv->cull();
    }
    //profile the draw call
    {
        //vpr::Guard<vpr::Mutex> val_guard( mValueLock );
        VPR_PROFILE_GUARD_HISTORY( "App::draw sv->draw", 20 );
        sv->draw();
    }

    //Allow trackball to grab frustum values to calculate FOVy
    sv->getCamera()->getProjectionMatrixAsFrustum(
        _left, _right, _bottom, _top, _near, _far );
    //The code below is not thread safe and will result in random results
    //in multithreaded use cases
    EnvironmentHandler::instance()->SetFrustumValues(
        _left, _right, _bottom, _top, _near, _far );
    
    //Screen capture code
    if( captureNextFrameForWeb )
    {
        vpr::Guard<vpr::Mutex> val_guard( mValueLock );
        mSceneRenderToTexture->WriteImageFileForWeb(
            getScene(), sv.get(), m_filename );
        captureNextFrameForWeb = false;
    }

    glMatrixMode( GL_PROJECTION );
    glPopMatrix();

    glMatrixMode( GL_MODELVIEW );
    glPopMatrix();

    glPopAttrib();
    glPopAttrib();
    glPopAttrib();

    if( mRTT )
    {
        *mAlreadyRendered = true;
    }
}
////////////////////////////////////////////////////////////////////////////////
void App::update()
{
    const std::string tempCommandName = 
        m_vjobsWrapper->GetXMLCommand()->GetCommandName();
    // This code came from osgViewer::Viewer::setSceneData
    // The resize stuff is what is critical not sure how important it is
    if( !tempCommandName.compare( "veNetwork Update" ) )
    {
        // make sure that existing scene graph objects are 
        // allocated with thread safe ref/unref
        getScene()->setThreadSafeRefUnref(true);
        
        // update the scene graph so that it has enough GL object buffer 
        // memory for the graphics contexts that will be using it.
        getScene()->resizeGLObjectBuffers( 
            osg::DisplaySettings::instance()->getMaxNumberOfGraphicsContexts() );
    }
    // Update the frame stamp with information from this frame
    //frameStamp->setFrameNumber( getFrameNumber() );
    //frameStamp->setReferenceTime( getFrameTime().secd() );

    // Set up the time and frame number so time dependant things (animations, particle system)
    // function correctly
    mUpdateVisitor->setTraversalNumber( _frameNumber );
    mUpdateVisitor->setFrameStamp( mFrameStamp.get() );

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
////////////////////////////////////////////////////////////////////////////////
