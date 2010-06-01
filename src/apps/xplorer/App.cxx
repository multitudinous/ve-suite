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
#include "SceneGLTransformInfo.h"

#include <ves/xplorer/TextureBasedVizHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/environment/cfdQuatCamHandler.h>
#include <ves/xplorer/environment/cfdDisplaySettings.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/command/CommandManager.h>

#include <ves/xplorer/volume/cfdPBufferManager.h>

#ifdef MINERVA_GIS_SUPPORT
# include <ves/xplorer/minerva/MinervaManager.h>
#endif

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
#include <osg/CullSettings>
#include <osg/Version>

#include <osgDB/WriteFile>
#include <osgDB/ReadFile>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>

#include <osgUtil/SceneView>
#include <osgUtil/UpdateVisitor>
#include <osgUtil/Statistics>

// --- VR Juggler Includes --- //
#include <gmtl/Generate.h>
#include <gmtl/Coord.h>
#include <gmtl/Misc/MatrixConvert.h>
#include <gmtl/Xforms.h>

#include <vrj/Kernel/Kernel.h>

#include <vrj/Draw/OpenGL/DrawManager.h>

#include <vrj/Display/DisplayManager.h>
#include <vrj/Display/Viewport.h>

#include <vpr/Perf/ProfileManager.h>
#include <vpr/System.h>
#include <vpr/Thread/Thread.h>

#include <jccl/RTRC/ConfigManager.h>

#ifdef QT_ON

#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/conductor/qt/UIManager.h>

//// --- Qt Includes --- //
#include <QtGui/QApplication>
#include <QtGui/QPushButton>
#include <QtGui/QDialog>
#include <QtGui/QVBoxLayout>
#include <QtGui/QPlastiqueStyle>

#include <ves/conductor/qt/UIElementQt.h>
#include <ves/conductor/qt/MainWindow.h>

//// --- Boost includes --- //
#include <boost/bind.hpp>

#endif // QT_ON

#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAudio/SoundManager.h>
#endif //VE_SOUND

// --- STL Includes --- //
#include <iostream>

using namespace ves::open::xml;
using namespace ves::xplorer;
using namespace ves::xplorer::util;
using namespace ves::xplorer::volume;
using namespace ves::xplorer::network;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
App::App( int argc, char* argv[], bool enableRTT )
    :
    vrj::osg::App( vrj::Kernel::instance() ),
    isCluster( false ),
    m_captureNextFrame( false ),
    m_captureMovie( false ),
    mRTT( enableRTT ),
    mProfileCounter( 0 ),
    mLastFrame( 0 ),
    mLastTime( 0 )
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
    light_0->setPosition( osg::Vec4d( 0.0, 10000.0, 10000.0, 0.0 ) );
    //light_0->setDirection( osg::Vec3d( 0.0, 1.0, -1.0 ) );

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

    //Set the ortho2D( 0, 1, 0, 1, 0, 1 ) matrix
    gmtl::Matrix44d ortho2DMatrix;
    ortho2DMatrix.mState =
        gmtl::Matrix44d::AFFINE | gmtl::Matrix44d::NON_UNISCALE;
    ortho2DMatrix.mData[  0 ] =  2.0;
    ortho2DMatrix.mData[  5 ] =  2.0;
    ortho2DMatrix.mData[ 10 ] = -2.0;
    ortho2DMatrix.mData[ 12 ] = -1.0;
    ortho2DMatrix.mData[ 13 ] = -1.0;
    ortho2DMatrix.mData[ 14 ] = -1.0;

    //Set the identity matrix
    gmtl::Matrix44d identityMatrix;
    identityMatrix.mState = gmtl::Matrix44d::IDENTITY;

    //Set the zUp transformation matrix
    gmtl::Vec3d x_axis( 1.0, 0.0, 0.0 );
    gmtl::Matrix44d zUpMatrix = gmtl::makeRot< gmtl::Matrix44d >(
        gmtl::AxisAngled( gmtl::Math::deg2Rad( -90.0 ), x_axis ) );

    m_sceneGLTransformInfo = SceneGLTransformInfoPtr( new SceneGLTransformInfo(
        ortho2DMatrix, identityMatrix, zUpMatrix ) );

#ifdef QT_ON
    ves::xplorer::data::DatabaseManager::instance()->SetDatabasePath("/tmp/ves.db");
#endif // QT_ON
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
#ifdef MINERVA_GIS_SUPPORT
    ves::xplorer::minerva::MinervaManager::instance()->Clear();
#endif
    ves::xplorer::scenegraph::SceneManager::instance()->Shutdown();
    ves::xplorer::network::GraphicalPluginManager::instance()->UnRegisterExecutive();
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* App::getScene()
{
    //osgDB::writeNodeFile(
        //*this->_sceneManager->GetRootNode()->GetRawNode(), "C:/test.osg" );

    return ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();
}
////////////////////////////////////////////////////////////////////////////////
void App::contextInit()
{
    //vrj::OsgApp::contextInit();
    std::cout << "|\tContext initialized" << std::endl;
    const unsigned int unique_context_id =
        vrj::opengl::DrawManager::instance()->getCurrentContext();

    //Create new context specific scene viewer
    osg::ref_ptr< osgUtil::SceneView > new_sv( new osgUtil::SceneView() );
    //Configure the new viewer
    configSceneView( new_sv.get() );
    //Hard code the LOD setting to be something high for the time being
    new_sv->setLODScale( 0.01 );
    //Set the unique id for this particular context
    new_sv->getState()->setContextID( unique_context_id );
    //Add the tree to the scene viewer and set properties
    {
        vpr::Guard< vpr::Mutex > sv_guard( mValueLock );
        new_sv->getCamera()->setName( "SV Camera" );
        if( !mRTT )
        {
            //new_sv->setSceneData( getScene() );
            new_sv->getCamera()->addChild( getScene() );
        }
        else
        {
            //*m_skipDraw = false;
        }
        *mViewportsChanged = false;
        m_sceneGLTransformInfo->Initialize();
        mSceneRenderToTexture->InitializeRTT();
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
    newSceneViewer->setClearColor( osg::Vec4( 1.0, 0.0, 0.0, 0.0 ) );

    {
        vpr::Guard<vpr::Mutex> val_guard( mValueLock );
        // Needed for stereo to work.
#if ( ( OPENSCENEGRAPH_MAJOR_VERSION >= 2 ) && \
    ( OPENSCENEGRAPH_MINOR_VERSION >= 9 ) && \
    ( OPENSCENEGRAPH_PATCH_VERSION >= 6 ) )
        ///This change is required because of the commit on rev 10547
        ///and the changes to CullVisitor specifically I believe - mccdo
        ///Hopefully a permanent solution to the problem can be found.
        ///This code needs to be tested to see if multi-context/quad buffer stereo
        ///and other rendering forms still work.
        newSceneViewer->getCamera()->setDrawBuffer( GL_NONE );
        //newSceneViewer->getRenderStage()->setDrawBufferApplyMask( false );
        //newSceneViewer->getRenderStage()->setDrawBuffer( GL_NONE, false );
        //newSceneViewer->getCamera()->setReadBuffer(GL_BACK);
        newSceneViewer->getCamera()->setInheritanceMask( 
            newSceneViewer->getCamera()->getInheritanceMask() | 
            osg::CullSettings::DRAW_BUFFER );
#else
        newSceneViewer->getCamera()->setDrawBuffer(GL_NONE);
#endif
        newSceneViewer->setSmallFeatureCullingPixelSize( 10 );
    }

    ///With this code in culling culs the near and far planes. I believe
    ///we discovered this awhile ago but removed the comments about it.
    ///Please see cullvisitor (osg/CullSettings)
    // for the possible settings for this function.
    //This defaults to setting the near and far plane based on the
    //bounding volume.
    if( mRTT )
    {
        newSceneViewer->setComputeNearFarMode(
            osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
        newSceneViewer->getCamera()->setCullingActive( false );
    }
    else
    {
        newSceneViewer->setComputeNearFarMode(
            osgUtil::CullVisitor::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
    }

    //Set default viewport, projection matrix, and view matrix for each scene view
    newSceneViewer->setViewport( 0.0, 0.0, 1.0, 1.0 );
    newSceneViewer->setProjectionMatrix(
        m_sceneGLTransformInfo->GetIdentityMatrixOSG() );
    newSceneViewer->setViewMatrix( 
        m_sceneGLTransformInfo->GetIdentityMatrixOSG() );
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
//Taken from VR Juggler OSG App.h
//
// Function that is called once for each frame buffer of an OpenGL context.
// This function is executed after contextInit() (if needed) but before
// contextPreDraw().  It is called once per frame buffer (see note).
//
// @pre The OpenGL context has been set to the context for drawing.
// @post The application object has executed any commands that need to be
//        executed once per context, per buffer, per frame.
//
// @note This function is designed to be used when some task must be
//       performed only once per frame buffer (i.e., once for the left
//       buffer, once for the right buffer).  For example, the OpenGL clear
//       color should be defined and glClear(GL_COLOR_BUFFER_BIT) should be
//       called in this method.
//
void App::bufferPreDraw()
{
#if 0
    glClearColor( 1.0, 0.0, 0.0, 0.0 );
    glClear( GL_COLOR_BUFFER_BIT );
#endif
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
    //Check and see if we have osg plugins available
    {
        osgDB::ReaderWriter* osgReaderWriter = 
            osgDB::Registry::instance()->getReaderWriterForExtension( "osg" );
        osgDB::Registry::ReaderWriterList readerlist = 
            osgDB::Registry::instance()->getReaderWriterList();
        if( (readerlist.size() == 0) || !osgReaderWriter )
        {
            std::cerr << 
            "OpenSceneGraph plugins are not available. Please make sure the OSG loaders are in your path." 
            << std::endl;
            exit();
        }
    }

    std::cout << std::endl;
    std::cout << "| ***************************************************************** |" << std::endl;
    m_vjobsWrapper->InitCluster();
    //Need this loop here so manipulators know whether we are in desktop mode
    for( int i = 1; i < argc; ++i )
    {
        if( ( std::string( argv[ i ] ) == std::string( "-VESDesktop" ) ) && 
            ( argc >= i + 2 ) )
        {
            ves::xplorer::scenegraph::SceneManager::instance()->
                SetDesktopMode( true );
        }
    }

    //Define the rootNode, worldDCS, and lighting
    ves::xplorer::scenegraph::SceneManager::instance()->SetRootNode(
            mSceneRenderToTexture->GetGroup() );
    ves::xplorer::scenegraph::SceneManager::instance()->InitScene();
    ves::xplorer::scenegraph::SceneManager::instance()->ViewLogo( true );
    ves::xplorer::scenegraph::SceneManager::instance()->
        SetFrameStamp( mFrameStamp.get() );

    //Setup the light
    osg::ref_ptr< osg::StateSet > lightStateSet = 
        getScene()->getOrCreateStateSet();
    lightStateSet->setAssociatedModes( light_0.get(), osg::StateAttribute::ON );
    
    lightStateSet->setMode( GL_LIGHTING, osg::StateAttribute::ON );
    
    lightStateSet->
        setAttributeAndModes( light_model_0.get(), osg::StateAttribute::ON );
    
    getScene()->addChild( light_source_0.get() );
    
    // modelHandler stores the arrow and holds all data and geometry
    ModelHandler::instance()->InitScene();

    // navigation and cursor
    EnvironmentHandler::instance()->Initialize();
    for( int i = 1; i < argc; ++i )
    {
        if( ( std::string( argv[ i ] ) == std::string( "-VESDesktop" ) ) && 
            ( argc >= i + 2 ) )
        {
            EnvironmentHandler::instance()->
                SetDesktopSize( atoi( argv[ i + 1 ] ), atoi( argv[ i + 2 ] ) );
            ves::xplorer::scenegraph::SceneManager::instance()->
                SetDesktopMode( true );
        }
        else if( std::string( argv[ i ] ) == std::string( "-VESCluster" ) )
        {
            isCluster = true;
        }
    }
    EnvironmentHandler::instance()->InitScene();
    cfdQuatCamHandler::instance()->SetMasterNode( m_vjobsWrapper->IsMaster() );
    
    //Set rtt mode for devices
    ves::xplorer::scenegraph::SceneManager::instance()->SetRTT( mRTT );
    
    // create steady state visualization objects
    SteadyStateVizHandler::instance()->Initialize( std::string() );
    SteadyStateVizHandler::instance()->InitScene();

    //create the volume viz handler
    _start_tick = _timer.tick();
    _tbvHandler = ves::xplorer::TextureBasedVizHandler::instance();
    _tbvHandler->SetMasterNode( m_vjobsWrapper->IsMaster() );

    std::cout << "|  2. Initializing.................................... GraphicalPluginManager |" << std::endl;
    GraphicalPluginManager::instance()->Initialize(
        m_vjobsWrapper->naming_context, m_vjobsWrapper->child_poa );

    // This may need to be fixed
    this->m_vjobsWrapper->GetCfdStateVariables();
#ifdef QT_ON
    // Get or create UIManager
    ves::conductor::UIManager* m_UIManager = ves::conductor::UIManager::instance();

    // UIManager needs to know how big in pixels its projection area is
    cfdDisplaySettings* cDS = EnvironmentHandler::instance()->GetDisplaySettings();
    std::pair<int, int> res = cDS->GetScreenResolution();
    m_UIManager->SetRectangle( 0, res.first, 0, res.second );

    // Hand current root node UIManager so it can create UI subgraph
    m_UIManager->Initialize( getScene() );

    // Start up the UI thread
    std::cout << "Starting UI thread" << std::endl;
    vpr::Thread* thread;
    thread = new vpr::Thread(boost::bind(&App::LoadUI, this));
#endif
}
////////////////////////////////////////////////////////////////////////////////
void App::preFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::preFrame", 20 );
    vprDEBUG( vesDBG, 3 ) << "|App::preFrame" << std::endl << vprDEBUG_FLUSH;
    ///////////////////////
    {
        //Check and see if the orb has any work to do
        VPR_PROFILE_GUARD_HISTORY( "App::preFrame CheckORBWorkLoad", 20 );
        //m_vjobsWrapper->CheckORBWorkLoad();
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::preFrame EnvironmentHandler", 20 );
        //Sets the worldDCS before it is synced
        EnvironmentHandler::instance()->PreFrameUpdate();
    }
    ///////////////////////
    vprDEBUG( vesDBG, 3 ) << "|End App::preFrame" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::latePreFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame", 20 );
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
    ves::xplorer::command::CommandManager::instance()->LatePreFrameUpdate();

    const ves::open::xml::CommandPtr tempCommandPtr = 
        ves::xplorer::command::CommandManager::instance()->GetXMLCommand();

    if( tempCommandPtr )
    {
        std::string tempCommandName;
        tempCommandName = tempCommandPtr->GetCommandName();
        
        //Exit - must be called AFTER m_vjobsWrapper->PreFrameUpdate();
        if( tempCommandName == "EXIT_XPLORER" )
        {
            // exit App was selected
            std::cout << "|\tShutting down xplorer." << std::endl;
            VPR_PROFILE_RESULTS();
            PhysicsSimulator::instance()->SetIdle( true );
            m_vjobsWrapper->Cleanup();
            GraphicalPluginManager::instance()->UnloadPlugins();
            ves::xplorer::scenegraph::SceneManager::instance()->Shutdown();
            
            // Stopping kernel
            vrj::Kernel::instance()->stop(); 
        }
        else if( !tempCommandName.compare( "SCREEN_SHOT" ) )
        {
            m_captureNextFrame = true;
            tempCommandPtr->
            GetDataValuePair( "Filename" )->GetData( m_filename );
            mSceneRenderToTexture->SetImageCameraCallback( true, m_filename );
        }
        else if( !tempCommandName.compare( "MOVIE_CAPTURE" ) )
        {
            m_captureMovie = true;
            tempCommandPtr->
            GetDataValuePair( "Filename" )->GetData( m_filename );
            mSceneRenderToTexture->SetImageCameraCallback( m_captureMovie, m_filename );
        }
        else if( !tempCommandName.compare( "MOVIE_CAPTURE_OFF" ) )
        {
            m_captureMovie = false;
            mSceneRenderToTexture->SetImageCameraCallback( m_captureMovie, "" );
        }
    }
    
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame Framerate Calculations", 20 );
        float current_time = this->m_vjobsWrapper->GetSetAppTime( -1 );
        float deltaTime = 0;
        //This is order dependent
        //don't move above function call
        mFrameStamp->setFrameNumber( _frameNumber );
        mFrameStamp->setReferenceTime( current_time );
        mFrameStamp->setSimulationTime( current_time );

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
    CharacterController& characterController =
        SceneManager::instance()->GetCharacterController();
    {
        VPR_PROFILE_GUARD_HISTORY(
            "App::latePreFrame CharacterController::Move", 20 );
        //If the character controller is being used - manipulate the character
        //by the keyboard, head, or wand first. This should affect the 
        //character bullet matrix directly
        characterController.Move( mFrameDT );
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame PhysicsSimulator", 20 );
        PhysicsSimulator::instance()->UpdatePhysics( mFrameDT );
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY(
            "App::latePreFrame CharacterController::UpdateCamera", 20 );
        //Now that the character has been moved AND the simulation has calculated
        //the new position update the camera matrix with the new view data
        //based on what the character has done
        characterController.UpdateCamera();
        //Now that we are finished updating the view on the character and controller
        //the update callback on the character will be called to update the 
        //OSG rep for the character
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
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame GraphicalPluginManager", 20 );
        GraphicalPluginManager::instance()->PreFrameUpdate();
    }
    ///////////////////////
#ifdef MINERVA_GIS_SUPPORT
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame MinervaManager", 20 );
        ves::xplorer::minerva::MinervaManager::instance()->PreFrameUpdate();
    }
#endif
    ///////////////////////
    ///Grab nav data
    {
        mNavPosition = gmtl::convertTo< double >( 
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveNavSwitchNode()->GetMat() );
    }
    ///////////////////////
    
    ///////////////////////
    //profile the update call
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame update", 20 );
        update();
    }
    ///////////////////////

    ///Increment framenumber now that we are done using it everywhere
    _frameNumber += 1;
    mProfileCounter += 1;

    vprDEBUG( vesDBG, 3 ) << "|End App::latePreFrame" 
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::intraFrame()
{
    vprDEBUG( vesDBG, 3 ) << "|App::intraFrame" << std::endl << vprDEBUG_FLUSH;
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
    vprDEBUG( vesDBG, 3 ) << "|App::postFrame" << std::endl << vprDEBUG_FLUSH;

    time_since_start = _timer.delta_s( _start_tick, _timer.tick() );

    this->m_vjobsWrapper->GetSetAppTime( time_since_start );
    EnvironmentHandler::instance()->PostFrameUpdate();
    //this->m_vjobsWrapper->GetSetFrameNumber( _frameNumber++ );

    ///update the transient frame number on the master
    _tbvHandler->UpdateTransientFrame();
    GraphicalPluginManager::instance()->PostFrameUpdate();

    this->m_vjobsWrapper->GetCfdStateVariables();
    
    if( m_captureNextFrame )
    {
        mSceneRenderToTexture->SetImageCameraCallback( false, "" );
        m_captureNextFrame = false;
    }

    vprDEBUG( vesDBG, 3 ) << "|End App::postFrame" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::contextPreDraw()
{
    //std::cout << "----------contextPreDraw-----------" << std::endl;
    VPR_PROFILE_GUARD_HISTORY( "App::contextPreDraw", 20 );

    if( !(*mViewportsChanged) && (_frameNumber > 3) )
    {
        if( jccl::ConfigManager::instance()->isPendingStale() )
        {            
            vpr::Guard< vpr::Mutex > val_guard( mValueLock );
            mSceneRenderToTexture->InitScene( (*sceneViewer)->getCamera() );
            update();

            if( mRTT )
            {
                vpr::System::msleep( 200 );  // thenth-second delay
                //*m_skipDraw = true;
            }
            *mViewportsChanged = true;
        }
    }
    
    ///Context specific updates for models that are loaded
    ves::xplorer::ModelHandler::instance()->ContextPreDrawUpdate();

    ///Adjust settings on the SceneView
    ///Info from Paul Martz below:
    ///OSG automatically computes near and far values to obtain the largest
    ///near/far ratio possible in order to maximize depth buffer precision. It
    ///performs this computation by first calculating the near and far values based
    ///on bounding spheres (or bounding boxes of Drawables). Note that if your
    ///eyepoint is in the scene, the computed near value might be negative - behind
    ///the eye! Then OSG computes another near plane value using a default near/far
    ///ration of 0.0005. In other words, it multiplies the computed far plane value
    ///by 0.0005. Then it uses the max of the two near values as the actual near
    ///plane. (This gets around the "negative" problem.) The bottom line: The
    ///computed near plane is always at least as big as the computed far multiplied
    ///by OSG's near/far ratio (which defaults to 0.0005).
    const ves::open::xml::CommandPtr tempCommandPtr = 
        ves::xplorer::command::CommandManager::instance()->GetXMLCommand();

    if( tempCommandPtr )
    {
        const std::string tempCommandName = 
            tempCommandPtr->GetCommandName();
        if( !tempCommandName.compare( "CHANGE_NEAR_FAR_RATIO" ) )
        {
            double nearFar;
            tempCommandPtr->
                GetDataValuePair( "Near Far Ratio" )->GetData( nearFar );
            (*sceneViewer)->setNearFarRatio( nearFar );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::draw()
{
    if( mRTT )
    {
        if( !mSceneRenderToTexture->CameraConfigured() )
        {
            return;
        }

        /*if( *m_skipDraw )
        {
            *m_skipDraw = false;
            return;
        }*/
    }

    //std::cout << "----------Draw-----------" << std::endl;
    VPR_PROFILE_GUARD_HISTORY( "App::draw", 20 );
    glClear( GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );

    //Users have reported problems with OpenGL reporting stack underflow
    //problems when the texture attribute bit is pushed here, so we push all
    //attributes *except* GL_TEXTURE_BIT.
    glPushAttrib( GL_ALL_ATTRIB_BITS & ~GL_TEXTURE_BIT );
    glPushAttrib( GL_TRANSFORM_BIT );
    glPushAttrib( GL_VIEWPORT_BIT );

    glMatrixMode( GL_MODELVIEW );
    glPushMatrix();

    glMatrixMode( GL_PROJECTION );
    glPushMatrix();

    //Get context specific scene viewer
    osg::ref_ptr< osgUtil::SceneView > sv = ( *sceneViewer );
    vprASSERT( sv.get() != NULL );

    sv->setLODScale( EnvironmentHandler::instance()->GetGlobalLODScale() );

    //The OpenGL Draw Manager that we are rendering for
    //Get the view matrix and the frustum from the draw manager
    vrj::opengl::DrawManager* glDrawManager =
        static_cast< vrj::opengl::DrawManager* >( getDrawManager() );
    //vprASSERT( glDrawManager != NULL );
    const vrj::opengl::UserData* userData = glDrawManager->currentUserData();
    const vrj::ViewportPtr viewport = userData->getViewport();
    const vrj::ProjectionPtr project = userData->getProjection();
    const vrj::Frustum frustum = project->getFrustum();

    //Get the frustum values
    double l = frustum[ vrj::Frustum::VJ_LEFT ];
    double r = frustum[ vrj::Frustum::VJ_RIGHT ];
    double b = frustum[ vrj::Frustum::VJ_BOTTOM ];
    double t = frustum[ vrj::Frustum::VJ_TOP ];
    double n = frustum[ vrj::Frustum::VJ_NEAR ];
    double f = frustum[ vrj::Frustum::VJ_FAR ];

    //Get and set the GLTransformInfo associated w/ this viewport and context
    scenegraph::GLTransformInfoPtr glTI =
        m_sceneGLTransformInfo->GetGLTransformInfo( viewport );
    if( glTI )
    {
#ifdef QT_ON
        //FIXME: This is probably dangerous in a multi-context enviroment
        {
            osg::Matrixd inverseVPW = glTI->GetVPWMatrixOSG();
            inverseVPW.invert( inverseVPW );
            ves::conductor::UIManager::instance()->SetProjectionMatrix( inverseVPW );
        }
#endif
        //Get the projection matrix
        glTI->UpdateFrustumValues( l, r, b, t, n, f );
        const osg::Matrixd projectionMatrixOSG = glTI->GetProjectionMatrixOSG();

        //Get the view matrix from vrj and transform into z-up land
        const gmtl::Matrix44d vrjViewMatrix =
            gmtl::convertTo< double >( project->getViewMatrix() ) *
            m_sceneGLTransformInfo->GetZUpMatrix();
        //Multiply by the camera matrix (mNavPosition)
        glTI->UpdateViewMatrix( vrjViewMatrix, mNavPosition );
        const osg::Matrixd viewMatrixOSG = glTI->GetViewMatrixOSG();

        if( mRTT )
        {
            osg::ref_ptr< osg::Camera > camera =
                mSceneRenderToTexture->GetCamera( viewport );
            if( camera.valid() )
            {
                sv->setViewport(
                    0.0, 0.0, glTI->GetWindowWidth(), glTI->GetWindowHeight() );
                camera->setProjectionMatrix( projectionMatrixOSG );
                camera->setViewMatrix( viewMatrixOSG );
            }
        }
        else
        {
            sv->setViewport(
                glTI->GetViewportOriginX(), glTI->GetViewportOriginY(),
                glTI->GetViewportWidth(), glTI->GetViewportHeight() );
            sv->setProjectionMatrix( projectionMatrixOSG );
            sv->setViewMatrix( viewMatrixOSG );
        }
    }
    else
    {
        //error output
        vprDEBUG( vesDBG, 1 ) << "App::draw(): invalid transfom info!!!"
                              << std::endl << vprDEBUG_FLUSH;
    }

    //Draw the scene
    //NOTE: It is not safe to call osgUtil::SceneView::update() here; it
    //should only be called by a single thread. The equivalent of calling
    //osgUtil::SceneView::update() is in vrj::OsgApp::update()
    //Profile the cull call
    {
        //vpr::Guard< vpr::Mutex > sv_guard( mValueLock );
        VPR_PROFILE_GUARD_HISTORY( "App::draw sv->cull", 20 );
        //Not sure if it should be used - came from osgViewer::Renderer::cull/draw
        //sv->inheritCullSettings( *(sv->getCamera()) );
        sv->cull();
    }
    //Profile the draw call
    {
        VPR_PROFILE_GUARD_HISTORY( "App::draw sv->draw", 20 );
        sv->draw();
    }

    if( glTI )
    {
        //Get the frustum planes based on the current bounding volume of the scene
        sv->getCamera()->getProjectionMatrixAsFrustum( l, r, b, t, n, f );
        //This code will go away eventually
        //The code below is not thread safe and will result in random results
        //in multithreaded use cases
        EnvironmentHandler::instance()->SetFrustumValues( l, r, b, t, n, f );
        //Recalculate the projection matrix from the new frustum values
        glTI->UpdateFrustumValues( l, r, b, t, n, f );
    }

    glMatrixMode( GL_PROJECTION );
    glPopMatrix();

    glMatrixMode( GL_MODELVIEW );
    glPopMatrix();

    glPopAttrib();
    glPopAttrib();
    glPopAttrib();

    //GLenum errorEnum = glGetError();
    //vprDEBUG( vesDBG, 3 ) <<  << errorEnum & GL_NO_ERROR 
    //    << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::update()
{
    vprDEBUG( vesDBG, 3 ) <<  "|\tApp LatePreframe Update" 
        << std::endl << vprDEBUG_FLUSH;
    /*const std::string tempCommandName = 
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
    }*/
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
    //getScene()->accept( *mUpdateVisitor.get() );
    
    //if( mRTT )
    {
        mSceneRenderToTexture->Update( mUpdateVisitor.get(), 
            ves::xplorer::command::CommandManager::instance()->GetXMLCommand() );
    }
    // now force a recompute of the bounding volume while we are still in
    // the read/write app phase, this should prevent the need to recompute
    // the bounding volumes from within the cull traversal which may be
    // multi-threaded.
    getScene()->getBound();
    
#ifdef VE_SOUND
    m_listenerPosition.set( mNavPosition.getData() );
    osgAudio::SoundManager::instance()->setListenerMatrix( listenerPosition );    
#endif
    vprDEBUG( vesDBG, 3 ) <<  "|\tEnd App LatePreframe Update" 
        << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::LoadUI()
{
    // This entire method should be run in its own thread since it blocks

#ifdef QT_ON
    // Create the Qt application event subsystem
    QApplication::setDesktopSettingsAware(true);
    QApplication a( argc, argv );

    // Get or create UIManager
    ves::conductor::UIManager* m_UIManager =
            ves::conductor::UIManager::instance();

    // Wrap the widget in a UIElement
    ves::conductor::UIElement* element = new ves::conductor::UIElementQt();
    QWidget* mainUIWidget = new MainWindow(0);
    
    // Since we're using an mdi-able MainWindow as the main widget, we make both 
    // it and the UIManager's projection take up the entire viewable area of
    // the GL window
    cfdDisplaySettings* cDS =
                           EnvironmentHandler::instance()->GetDisplaySettings();
    std::pair<int, int> res = cDS->GetScreenResolution();
    m_UIManager->SetRectangle( 0, res.first, 0, res.second );

    mainUIWidget->resize( res.first, res.second );
    static_cast< ves::conductor::UIElementQt* >
                                           (element)->SetWidget( mainUIWidget );
   
    m_UIManager->AddElement( element );

    // Begin running the Qt subsystem
    std::cout << "...Run Qt application" << std::endl;
    a.exec();
    std::cout << "...Ended Qt application" << std::endl;
#endif // QT_ON
}
////////////////////////////////////////////////////////////////////////////////
