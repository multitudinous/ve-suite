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
#include "App.h"
#include "VjObsWrapper.h"
#include "SceneRenderToTexture.h"
#include "SceneGLTransformInfo.h"
#include "KeyPressEater.h"
#include "VESQtApplication.h"

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

#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/eventmanager/EventMapper.h>
#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/SignalWrapper.h>
#include <ves/conductor/qt/UIManager.h>
#include <ves/conductor/qt/UIElementQt.h>
#include <ves/conductor/qt/MainWindow.h>

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

// --- BackdropFX Includes --- //
#include <backdropFX/Version.h>
#include <backdropFX/Manager.h>
#include <backdropFX/RTTViewport.h>

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

//// --- Qt Includes --- //
#include <QtGui/QApplication>
#include <QtGui/QPushButton>
#include <QtGui/QDialog>
#include <QtGui/QVBoxLayout>
#include <QtCore/QDir>

//// --- Boost includes --- //
#define BOOST_FILESYSTEM_VERSION 3
#include <boost/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/version.hpp>
#include <boost/system/error_code.hpp>
#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAudio/SoundManager.h>
#endif //VE_SOUND

// --- STL Includes --- //
#include <iostream>

using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::xplorer;
using namespace ves::xplorer::util;
using namespace ves::xplorer::volume;
using namespace ves::xplorer::network;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
App::App( int argc, char* argv[], bool enableRTT )
    :
    vrj::osg::App( vrj::Kernel::instance() ),
    svUpdate( false ),
    isCluster( false ),
    m_captureNextFrame( false ),
    m_captureMovie( false ),
    mRTT( enableRTT ),
    m_uiInitialized( false ),
    m_MouseInsideUI( true ),
    mProfileCounter( 0 ),
    mLastFrame( 0 ),
    mLastTime( 0 ),
    mLastQtLoopTime( 0.0 ),
    m_logger( Poco::Logger::get( "xplorer.App" ) ),
    m_windowIsOpen( false ),
    m_nearFarRatio( 0.0005 ),
    m_frameSetNearFarRatio( 0 ),
    m_processSignals( false ),
    m_exitApp( false )
{
    m_logStream = ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) );
    LOG_INFO("Starting App");

    osg::Referenced::setThreadSafeReferenceCounting( true );
    osg::DisplaySettings::instance()->setMaxNumberOfGraphicsContexts( 20 );
    mFrameStamp = new osg::FrameStamp();
    mUpdateVisitor = new osgUtil::UpdateVisitor();
    mFrameStamp->setReferenceTime( 0.0 );
    mFrameStamp->setFrameNumber( 0 );

    _tbvHandler = 0;
#ifdef _PBUFFER
    _pbuffer = 0;
#endif
    _frameNumber = 0;

    /*
    std::vector< bool* >* tempData = m_setNearFarRatio.getDataVector();
    for( size_t i = 0; i < tempData->size(); ++i )
    {
        *(tempData->at( i )) = false;
    }
    */

    this->argc = argc;
    this->argv = argv;

    mSceneRenderToTexture =
        SceneRenderToTexturePtr( new SceneRenderToTexture( mRTT ) );

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

    //Set the current database file and clear it out in case it contains data
    //from a previous session
    std::string dbPath;
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
    boost::filesystem::path tempPath;
    try
    {
        tempPath = boost::filesystem::temp_directory_path();
    }
    catch( boost::filesystem::filesystem_error& ec )
    {
        std::cout << ec.what() << std::endl;
    }
    char* logName = std::getenv("LOGNAME");
    std::string logNameStr( "ves-" );
    if( !logName )
    {
        logNameStr.append( "debug" );
    }
    else
    {
        logNameStr.append( logName );
    }
    tempPath /= logNameStr;
    // Create ves-LOGNAME subdir if needed
    if( !boost::filesystem::exists(tempPath) )
    {
        boost::filesystem::create_directory(tempPath);
    }
    tempPath /= "ves.db";
    dbPath = tempPath.string();
#else
#if defined(_MSC_VER)
    dbPath = "C:/Temp";
#else
    dbPath = "/tmp";
#endif // _MSC_VER
    dbPath.append( "/ves.db" );
#endif // BOOST_VERSION
    ves::xplorer::data::DatabaseManager::instance()->SetDatabasePath( dbPath );
    ves::xplorer::data::DatabaseManager::instance()->ResetAll();

    // Register signal(s) with EventManager
    eventmanager::EventManager::instance()->RegisterSignal(
    new eventmanager::SignalWrapper< latePreFrame_SignalType >( &mLatePreFrame ),
    "App.LatePreFrame");

    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< exit_SignalType >( &m_exitSignal ),
        "App.Exit");

    CONNECTSIGNALS_2( "%NearFarRatio", void( bool const&, double const& ),
                          &ves::xplorer::App::SetNearFarRatio,
                          m_connections, any_SignalType, normal_Priority );  
}
////////////////////////////////////////////////////////////////////////////////
App::~App()
{
    LOG_INFO( "Quitting App" );
#if defined( _DARWIN )
    if( m_signalLock.test() )
    {
        m_signalLock.release();
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
void App::exit()
{
    m_exitApp = true;
    m_exitSignal(m_exitApp);

#if defined( _DARWIN )
    if( m_signalLock.test() )
    {
        m_signalLock.release();
    }
#endif
    //Profiling guard used by vrjuggler
    VPR_PROFILE_RESULTS();
    ves::xplorer::data::DatabaseManager::instance()->Shutdown();
    std::cout << "|\tApp is now exiting." << std::endl;
    m_vjobsWrapper->Cleanup();
    GraphicalPluginManager::instance()->UnloadPlugins();
    ves::xplorer::scenegraph::SceneManager::instance()->Shutdown();
#ifdef MINERVA_GIS_SUPPORT
    ves::xplorer::minerva::MinervaManager::instance()->Clear();
#endif
    ves::xplorer::network::GraphicalPluginManager::instance()->UnRegisterExecutive();

    if( !mRTT )
    {
        //Cleanup backdropFX
        backdropFX::Manager::instance( true );
    }
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
        //if( !mRTT )
        {
            //new_sv->getCamera()->addChild( getScene() );
        }
        //else
        {
            //*m_skipDraw = false;
        }
        *mViewportsChanged = false;
        m_sceneGLTransformInfo->Initialize();
        mSceneRenderToTexture->InitializeRTT();
    }

    ( *sceneViewer ) = new_sv;

    ///Initialize the context specific flags
    //*m_setNearFarRatio = false;
    
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
    //if( mRTT )
    {
        //newSceneViewer->setComputeNearFarMode(
            //osgUtil::CullVisitor::DO_NOT_COMPUTE_NEAR_FAR );
        //newSceneViewer->getCamera()->setCullingActive( false );
    }
    //else
    {
        //newSceneViewer->setComputeNearFarMode(
            //osgUtil::CullVisitor::COMPUTE_NEAR_FAR_USING_BOUNDING_VOLUMES );
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
    vprDEBUG( vesDBG, 0 ) << "|App::initScene" << std::endl << vprDEBUG_FLUSH;
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

    //Set rtt mode for devices
    SceneManager::instance()->SetRTT( mRTT );
    //Define the rootNode, worldDCS, and lighting
    SceneManager::instance()->InitScene();
    SceneManager::instance()->ViewLogo( true );
    SceneManager::instance()->SetFrameStamp( mFrameStamp.get() );

    // modelHandler stores the arrow and holds all data and geometry
    ModelHandler::instance()->InitScene();

    //Initialize DeviceHandler
    DeviceHandler::instance()->Initialize();
    //Tell the scenemanager about the devices
    SceneManager::instance()->SetDeviceHandlerGroup(
        DeviceHandler::instance()->GetDeviceGroup() );

    // navigation and cursor
    EnvironmentHandler::instance()->Initialize();
    for( int i = 1; i < argc; ++i )
    {
        if( ( std::string( argv[ i ] ) == std::string( "-VESDesktop" ) ) && 
            ( argc > i + 2 ) )
        {
            EnvironmentHandler::instance()->
                SetDesktopSize( atoi( argv[ i + 1 ] ), atoi( argv[ i + 2 ] ) );
        }
        else if( std::string( argv[ i ] ) == std::string( "-VESCluster" ) )
        {
            isCluster = true;
        }
    }
    EnvironmentHandler::instance()->InitScene();
    cfdQuatCamHandler::instance()->SetMasterNode( m_vjobsWrapper->IsMaster() );

    //Tell scenemanager if we are the master node
    ves::xplorer::scenegraph::SceneManager::instance()->
        SetMasterNode( m_vjobsWrapper->IsMaster() );

    // create steady state visualization objects
    SteadyStateVizHandler::instance()->InitScene();

    //create the volume viz handler
    _start_tick = _timer.tick();
    _tbvHandler = ves::xplorer::TextureBasedVizHandler::instance();
    _tbvHandler->SetMasterNode( m_vjobsWrapper->IsMaster() );

    GraphicalPluginManager::instance()->Initialize(
        m_vjobsWrapper->naming_context, m_vjobsWrapper->child_poa );

    //This may need to be fixed
    m_vjobsWrapper->GetCfdStateVariables();

    //Get or create UIManager
    ves::conductor::UIManager* m_UIManager = ves::conductor::UIManager::instance();

    //Hand current root node UIManager so it can create UI subgraph
    m_UIManager->Initialize( getScene() );

    //Start up the UI thread
    //m_qtUIThread = new vpr::Thread( boost::bind( &App::LoadUI, this ) );
}
////////////////////////////////////////////////////////////////////////////////
void App::preFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::preFrame", 20 );
    vprDEBUG( vesDBG, 3 ) << "|App::preFrame" << std::endl << vprDEBUG_FLUSH;

    {
        //Check and see if the orb has any work to do
        //VPR_PROFILE_GUARD_HISTORY( "App::preFrame CheckORBWorkLoad", 20 );
        //m_vjobsWrapper->CheckORBWorkLoad();
    }

    {
        VPR_PROFILE_GUARD_HISTORY( "App::preFrame EnvironmentHandler", 20 );
        //Sets the worldDCS before it is synced
        EnvironmentHandler::instance()->PreFrameUpdate();
    }

#ifndef _DARWIN
    if( !m_uiInitialized )
    {
        if( jccl::ConfigManager::instance()->isPendingStale() && m_windowIsOpen )
        {
            preRun();
        }
    }
#endif
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
        const std::string tempCommandName = tempCommandPtr->GetCommandName();
        
        //Exit - must be called AFTER m_vjobsWrapper->PreFrameUpdate();
        if( tempCommandName == "EXIT_XPLORER" )
        {
            // exit App was selected
            std::cout << "|\tShutting down xplorer." << std::endl;
            VPR_PROFILE_RESULTS();
            // Stopping kernel
            vrj::Kernel::instance()->stop();
            PhysicsSimulator::instance()->SetIdle( true );
            m_exitApp = true;
            return;
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
        
        double tempSimTime = mFrameStamp->getSimulationTime();

        mFrameDT = current_time - mLastFrameTime;
        mLastFrameTime = current_time;
        
        mFrameStamp->setSimulationTime( tempSimTime + mFrameDT );

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
        scenegraph::SceneManager::instance()->LatePreFrameUpdate();
        //Need to figure out why osg::LightSource::ABSOLUTE_RF
        //does not work in a multi-context environment
        //jbkoch - Since in the CAVE each view is 90 degrees apart,
        //we do not want the light position relative to those matrix stacks,
        //we want it relative to the camera matrix
        gmtl::Matrix44d tempNavMatrix =
            scenegraph::SceneManager::instance()->GetInvertedNavMatrix();
        gmtl::Vec4d tempVec( 0.0, -10000.0, 10000.0, 0.0 );
        tempVec = tempNavMatrix * tempVec;
        osg::Vec4d position( tempVec[ 0 ], tempVec[ 1 ], tempVec[ 2 ], 0.0 );
        mSceneRenderToTexture->GetLight0()->setPosition( position );
        //Sneaky way to set uniform defined in
        //Access to these types of uniforms needs to be discussed with Paul
        if( !mRTT )
        {
            osg::StateSet* stateset =
                mSceneRenderToTexture->GetRootGroup()->getStateSet();
            if( stateset )
            {
                osg::Uniform* uniform =
                    stateset->getUniform( "bdfx_lightSource[0].position" );
                if( uniform )
                {
                    uniform->set( position );
                }
            }
        }
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
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame EventMapper", 20 );
        ves::xplorer::eventmanager::EventMapper::instance()->LatePreFrameUpdate();
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
    // Signal allowing other listeners to perform processing synced to draw
    {
        mLatePreFrame();
    }
    ///////////////////////


#if !defined( _DARWIN )
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame update", 20 );
        ///Because we do our event processing here for Qt we get automagical 
        ///sync between the draw thread and the UI thread. This call 
        ///essentially tells Qt to process all of its events at this moment. 
        ///If this is changed in the future where this call is being run in a 
        ///different thread then we may need to take more care in handling the 
        ///sync issues between our UI and the render thread.
        ///Note: On Mac the runLoop method is called by the NSApplication
        ///      delegate VESDelegate. On Mac the runLoop method is executed
        ///      from the primordial thread. This is different than the none
        ///      Mac case where it is being executed in the VR Juggler frame
        ///      thread.
        runLoop();
    }
#else
    ///This code lets Qt Application process events generated by the runLoop
    ///method.
    if( m_uiInitialized )
    {
        ///We need to guard these calls with a timer because on Mac unlike the 
        ///other platforms the runLoop does not completely control the event
        ///processing therefore we need to block events with this call as well.
        bool oneHertzUpdate = ( time_since_start - mLastQtLoopTime ) > 0.9999f;
        if( m_MouseInsideUI || oneHertzUpdate )
        {
            //mLastQtLoopTime = time_since_start;
            m_signalLock.release();
            vprDEBUG( vesDBG, 3 ) << "|\tApp::latePreFrame process signals"
                << std::endl << vprDEBUG_FLUSH;
            m_signalLock.acquire();
        }
    }
#endif 
    
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

    //Do nothing here, usually slows things down
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

    m_vjobsWrapper->GetSetAppTime( time_since_start );
    EnvironmentHandler::instance()->PostFrameUpdate();
    //this->m_vjobsWrapper->GetSetFrameNumber( _frameNumber++ );

    ///update the transient frame number on the master
    _tbvHandler->UpdateTransientFrame();
    GraphicalPluginManager::instance()->PostFrameUpdate();

    m_vjobsWrapper->GetCfdStateVariables();

    if( m_captureNextFrame )
    {
        mSceneRenderToTexture->SetImageCameraCallback( false, "" );
        m_captureNextFrame = false;
    }

    SceneManager::instance()->PostFrameUpdate();

    vprDEBUG( vesDBG, 3 ) << "|End App::postFrame"
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parrallel in a multiple context situation
///so setting variables should not be done here
void App::contextPreDraw()
{
    //std::cout << "----------contextPreDraw-----------" << std::endl;
    VPR_PROFILE_GUARD_HISTORY( "App::contextPreDraw", 20 );
    //Try to determine when we have a valid context
    if( !m_windowIsOpen )
    {
        //Get the view matrix and the frustum from the draw manager
        vrj::opengl::DrawManager* glDrawManager =
            static_cast< vrj::opengl::DrawManager* >( getDrawManager() );
        const vrj::opengl::UserData* userData = glDrawManager->currentUserData();
        const vrj::opengl::WindowPtr window = userData->getGlWindow();
        m_windowIsOpen = window->isOpen();
    }

    if( !(*mViewportsChanged) )
    {
        if( jccl::ConfigManager::instance()->isPendingStale() && m_windowIsOpen )
        {
            vpr::Guard< vpr::Mutex > val_guard( mValueLock );
            mSceneRenderToTexture->InitScene( (*sceneViewer)->getCamera() );
            update();

            //if( mRTT )
            {
                //vpr::System::msleep( 200 );  // thenth-second delay
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
    if( m_frameSetNearFarRatio == _frameNumber )
    {
        (*sceneViewer)->setNearFarRatio( m_nearFarRatio );
        //*m_setNearFarRatio = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
///Remember that this is called in parallel in a multiple context situation
///so setting variables should not be done here
void App::draw()
{
    if( !mSceneRenderToTexture->CameraConfigured() )
    {
        return;
    }

    //std::cout << "----------Draw-----------" << std::endl;
    VPR_PROFILE_GUARD_HISTORY( "App::draw", 20 );
    glClear( GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT );

    //Users have reported problems with OpenGL reporting stack underflow
    //problems when the texture attribute bit is pushed here,
    //so we push all attributes *except* GL_TEXTURE_BIT
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
        ///Remember that the VR Juggler near/far values are hard coded to 
        ///0.1 and 10000. With OSG set to auto compute the near and far planes
        ///these values are overriden. If we use GLTransformInfo just after
        ///these values are set the projection matrix will not reflect
        ///what OSG is actually using for rendering.
        glTI->UpdateFrustumValues( l, r, b, t, n, f );
        //Get the projection matrix
        const osg::Matrixd projectionMatrixOSG = glTI->GetProjectionMatrixOSG();

        //Get the view matrix from vrj and transform into z-up land
        const gmtl::Matrix44d vrjViewMatrix =
            gmtl::convertTo< double >( project->getViewMatrix() ) *
            m_sceneGLTransformInfo->GetZUpMatrix();
        //Multiply by the camera matrix (mNavPosition)
        glTI->UpdateViewMatrix( vrjViewMatrix, mNavPosition );
        const osg::Matrixd viewMatrixOSG = glTI->GetViewMatrixOSG();

        //Get the view matrix from a centered eye position
        m_sceneGLTransformInfo->CalculateCenterViewMatrix( project );

        //Can't set viewport larger than fbo texture for rtt camera
        //We have to set the viewport for the frame buffer
        //Would like to inherit viewport from sv all way down but not possible
        //The rtt camera must be ABSOLUTE_RF because of implementation of
        //AutoTransform::computeLocalToWorldMatrix() and
        //AbsoluteModelTransform::computeLocalToWorldMatrix()
        //If it is not absolute, Physics and Manipulators will be broken
        sv->setProjectionMatrix( projectionMatrixOSG );
        sv->setViewMatrix( viewMatrixOSG );
        if( mRTT )
        {
            osg::Camera* camera = mSceneRenderToTexture->GetPostProcessCamera();
            camera->setViewport(
                glTI->GetViewportOriginX(), glTI->GetViewportOriginY(),
                glTI->GetViewportWidth(), glTI->GetViewportHeight() );
        }
        else
        {
            sv->setViewport( &( glTI->GetBdfxRTTViewport() ) );
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
                          //<< std::endl << vprDEBUG_FLUSH;
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
    Poco::Logger& conductorLoggerInit = Poco::Logger::get( "conductor" );
    boost::ignore_unused_variable_warning( conductorLoggerInit );

    vprDEBUG( vesDBG, 2 ) << "|\tApp LoadUI" << std::endl << vprDEBUG_FLUSH;

    //Request connection to UIManager.EnterLeaveUI signal
    CONNECTSIGNAL_1( "UIManager.EnterLeaveUI", void( bool ), &App::UIEnterLeave,
                     mConnections, highest_Priority );

    // Create the Qt application event subsystem
    QApplication::setDesktopSettingsAware(true);
    
#if !defined( _DARWIN )
    m_qtApp = new QApplication( argc, argv, 1 );
#else
    QApplication::setAttribute(Qt::AA_MacPluginApplication);
    m_qtApp = new ves::xplorer::VESQtApplication( argc, argv, this );
#endif

#ifdef VES_QT_RENDER_DEBUG
    QPushButton*  button = new QPushButton("Test");
    button->show();
    m_uiInitialized = true;
    return;
#endif
    //Get or create UIManager
    ves::conductor::UIManager* m_UIManager =
        ves::conductor::UIManager::instance();

    // Wrap the widget in a UIElement
    ves::conductor::UIElementQt* element = new ves::conductor::UIElementQt();
    QWidget* mainUIWidget = new MainWindow( 0 );

    // Make UIManager's projection take up the entire viewable area of
    // the GL window
    // Now lets find out what we actually got back from the OS
    vrj::DisplayManager* displayManager = vrj::DisplayManager::instance();
    const std::vector< vrj::DisplayPtr >& displays = 
        displayManager->getActiveDisplays();
    int originX = 0; int originY = 0; int width = 0; int height = 0;
    for( size_t i = 0; i < displays.size(); ++i )
    {
        vrj::DisplayPtr display = displays.at( i );
        display->getOriginAndSize( originX, originY, width, height );
    }
    std::cout << "|\tWindow value: " << width << " " 
        << height << std::endl << std::flush;
    
    if( !ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    { 
        ///These values must match the values in UIElement on line 105
        width = 600;
        height = 967;
    }
    ///Only the height value is used to enable flipping the y value to put
    ///it in qt space.
    m_UIManager->SetRectangle( 0, width, 0, height );

    element->SetInitialImageWidthAndHeight( 600, height );
    element->SetScreenDimensions( width, height );
    element->SetWidget( mainUIWidget );
    
    m_UIManager->AddElement( element );

    // Start the main UI minimized
    //Until we get minimize working with the new matrix stack tools this will
    //be disabled.
    //m_UIManager->MinimizeElement( element );

    m_uiInitialized = true;

    vprDEBUG( vesDBG, 2 ) << "|\tEnd App LoadUI" << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::UIEnterLeave( bool entered )
{
    m_MouseInsideUI = entered;
}
////////////////////////////////////////////////////////////////////////////////
void App::preRun()
{
    LoadUI();
}
////////////////////////////////////////////////////////////////////////////////
void App::runLoop()
{
    if( m_uiInitialized && !m_exitApp )
    {
        bool oneHertzUpdate = ( time_since_start - mLastQtLoopTime ) > 0.9999f;
        if( m_MouseInsideUI || oneHertzUpdate )
        {
            mLastQtLoopTime = time_since_start;
            //On mac this call reposts to cocoa
            m_qtApp->processEvents();// QEventLoop::DeferredDeletion );
            // Just using sendPostedEvents without processEvents does not push mouse
            // and keyboard events through. Using processEvents alone without sendPostedEvents
            // appears to work fine.
            //m_qtApp->sendPostedEvents(0, QEvent::DeferredDelete);
            //m_qtApp->sendPostedEvents(0, 0);
        }
#if defined( _DARWIN )
        else
        {
            vpr::Thread::yield();
        }
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void App::SetNearFarRatio( bool const& enable, double const& nearFar )
{
    /*std::vector< bool* >* tempData = m_setNearFarRatio.getDataVector();
    std::cout << tempData->size() << std::endl;
    for( size_t i = 0; i < tempData->size(); ++i )
    {
        *(tempData->at( i )) = true;
        std::cout << " here 2 " << std::endl;
    }*/
    m_frameSetNearFarRatio = _frameNumber + 1;
    
    if( enable )
    {
        m_nearFarRatio = nearFar;
    }
    else
    {
        m_nearFarRatio = 0.0005;
    }
}
#if defined( _DARWIN )
////////////////////////////////////////////////////////////////////////////////
bool App::AcquireQtLock()
{
    if( m_uiInitialized && !m_exitApp )
    {
        //return m_signalLock.tryAcquire();
        m_signalLock.acquire();
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void App::ReleaseQtLock()
{
    if( m_uiInitialized && !m_exitApp )
    {
        m_signalLock.release();
    }
}
////////////////////////////////////////////////////////////////////////////////
bool App::Test()
{
    if( m_uiInitialized && !m_exitApp )
    {
        return m_signalLock.test();
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
#endif
