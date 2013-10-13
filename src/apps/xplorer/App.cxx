/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include "SceneRenderToTexture.h"
#include "SceneGLTransformInfo.h"
//#include "KeyPressEater.h"
#include "VESQtApplication.h"

#include <ves/xplorer/TextureBasedVizHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/SteadyStateVizHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/environment/cfdDisplaySettings.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/volume/cfdPBufferManager.h>

#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/xplorer/eventmanager/EventMapper.h>
#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>
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

#include <osgwQuery/QueryBenchmarks.h>

#include <osgwMx/MxCore.h>

#include <osg/io_utils>

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
#include <vrj/Display/SurfaceProjection.h>
#include <vrj/Display/Frustum.h>

#include <vpr/Perf/ProfileManager.h>
#include <vpr/System.h>
#include <vpr/Thread/Thread.h>

#include <jccl/RTRC/ConfigManager.h>

//// --- Qt Includes --- //
#include <QtGui/QApplication>
#include <QtGui/QPushButton>
#include <QtGui/QDialog>
#include <QtGui/QVBoxLayout>
#include <QtGui/QStyle>
#include <QtGui/QStyleFactory>
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

// --- latticefx Includes --- //
#include <latticefx/core/Log.h>
#include <latticefx/core/LogMacros.h>
#include <latticefx/core/PagingThread.h>

using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::xplorer;
using namespace ves::xplorer::volume;
using namespace ves::xplorer::network;
using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
App::App( int argc, char* argv[], bool enableRTT, boost::program_options::variables_map vm, Poco::SplitterChannel* splitter )
    :
    vrj::osg::App( vrj::Kernel::instance() ),
    m_captureNextFrame( false ),
    m_captureMovie( false ),
    mRTT( enableRTT ),
    m_uiInitialized( false ),
    m_MouseInsideUI( true ),
    _frameNumber( 0 ),
    mProfileCounter( 0 ),
    mLastFrame( 0 ),
    mLastTime( 0. ),
    mLastFrameTime( 0. ),
    mFrameDT( 0. ),
    time_since_start( 0. ),
    mLastQtLoopTime( 0. ),
    m_numContexts( 0 ),
    m_numInitialized( 0 ),
    m_render( false ),
    m_logger( Poco::Logger::get( "xplorer.App" ) ),
    m_windowIsOpen( false ),
    m_nearFarRatio( 0.0005 ),
    m_frameSetNearFarRatio( 0 ),
    m_exitApp( false ),
    m_vm( vm ),
    m_logSplitter( splitter ),
    m_isMaster( true )
{
    m_logStream = ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) );
    LOG_INFO( "Starting App" );

#ifdef _DEBUG

	lfx::core::Log::instance()->setPriority( lfx::core::Log::PrioTrace,  lfx::core::Log::Console );
    //Log::instance()->setPriority( Log::PrioInfo, Log::Console );
    lfx::core::Log::instance()->setPriority( lfx::core::Log::PrioTrace, "lfx.core.page" );
#endif

    {
        //OSG specific settings
        osg::Referenced::setThreadSafeReferenceCounting( true );
        osg::DisplaySettings::instance()->setMaxNumberOfGraphicsContexts( 20 );

        mFrameStamp = new osg::FrameStamp();
        mUpdateVisitor = new osgUtil::UpdateVisitor();
        mFrameStamp->setReferenceTime( 0.0 );
        mFrameStamp->setFrameNumber( 0 );

        ///Setup caching so that textures will be cached across files
        //enum  	CacheHintOptions {
        //CACHE_NONE = 0, CACHE_NODES = 1<<0, CACHE_IMAGES = 1<<1, CACHE_HEIGHTFIELDS = 1<<2,
        //CACHE_ARCHIVES = 1<<3, CACHE_OBJECTS = 1<<4, CACHE_SHADERS = 1<<5, CACHE_ALL }
        osgDB::ReaderWriter::Options* opt = new osgDB::ReaderWriter::Options;
        opt->setObjectCacheHint( osgDB::ReaderWriter::Options::CACHE_IMAGES );
        osgDB::Registry::instance()->setOptions( opt );
    }

    m_isMaster = !vm[ "vrjslave" ].as<bool>();
    //bool clusterMode = !vm["vrjslave"].as<bool>() && !vm["vrjmaster"].as<bool>();
    //std::cout << " cluster mode " << clusterMode << std::endl;

    _tbvHandler = 0;
#ifdef _PBUFFER
    _pbuffer = 0;
#endif
    _frameNumber = 0;
#if defined( VES_QT_APP )
    m_loadingUILock.acquire();
#endif

    this->argc = argc;
    this->argv = argv;

    mSceneRenderToTexture =
        SceneRenderToTexturePtr( new SceneRenderToTexture( mRTT ) );
    if( vm.count( "DesktopWindowName" ) > 0 )
    {
        mSceneRenderToTexture->SetDesktopWindowName( vm[ "DesktopWindowName" ].as< std::string >() );
    }

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

    ///Setup the event mapper to initialize the default signals
    eventmanager::EventMapper::instance();

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
    char* logName = std::getenv( "LOGNAME" );
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
    if( !boost::filesystem::exists( tempPath ) )
    {
        boost::filesystem::create_directory( tempPath );
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
    std::cout << "DB Path : " << dbPath << std::endl;
    ves::xplorer::data::DatabaseManager::instance()->SetDatabasePath( dbPath );
    ves::xplorer::data::DatabaseManager::instance()->ResetAll();

    // Register signal(s) with EventManager
    switchwire::EventManager::instance()->RegisterSignal(
        ( &mLatePreFrame ),
        "App.LatePreFrame" );

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_exitSignal ),
        "App.Exit" );

    CONNECTSIGNALS_2( "%NearFarRatio", void( bool const&, double const& ),
                      &ves::xplorer::App::SetNearFarRatio,
                      m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
App::~App()
{
    LOG_INFO( "Quitting App" );
#if defined( VES_QT_APP )
    if( m_signalLock.test() )
    {
        m_signalLock.release();
    }

    if( m_loadingUILock.test() )
    {
        m_loadingUILock.release();
    }
#endif
}
////////////////////////////////////////////////////////////////////////////////
void App::exit()
{
    m_exitApp = true;
    m_exitSignal.signal( m_exitApp );

#if defined( VES_QT_APP )
    if( m_signalLock.test() )
    {
        m_signalLock.release();
    }

    if( m_loadingUILock.test() )
    {
        m_loadingUILock.release();
    }
#endif
    //Profiling guard used by vrjuggler
    VPR_PROFILE_RESULTS();
    ves::xplorer::data::DatabaseManager::instance()->Shutdown();
    std::cout << "|\tApp is now exiting." << std::endl;
    ves::xplorer::scenegraph::SceneManager::instance()->Shutdown();
    GraphicalPluginManager::instance()->UnloadPlugins();
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
    return ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();
}
////////////////////////////////////////////////////////////////////////////////
void App::contextInit()
{
    //vrj::OsgApp::contextInit();
    const unsigned int unique_context_id =
        vrj::opengl::DrawManager::instance()->getCurrentContext();
    std::cout << "|\tContext initialized " << unique_context_id << std::endl;

    //Create new context specific scene viewer
    osg::ref_ptr< osgUtil::SceneView > new_sv( new osgUtil::SceneView() );
    //Configure the new viewer
    configSceneView( new_sv.get() );
    //Set the unique id for this particular context
    new_sv->getState()->setContextID( unique_context_id );
    //Hard code the LOD setting to be something high for the time being
    new_sv->setLODScale( 0.01 );
    //Add the tree to the scene viewer and set properties
    {
        vpr::Guard< vpr::Mutex > sv_guard( mValueLock );
        osg::Camera* camera = new_sv->getCamera();
        camera->setName( "SV Camera" );
        camera->setPreDrawCallback( new osgwQuery::InitCallback() );

        *mViewportsChanged = false;
        m_sceneGLTransformInfo->Initialize();
        mSceneRenderToTexture->InitializeRTT();
        
        //Setup the lfx basic texture sizing
        {
            const vrj::ViewportPtr viewport =
                vrj::opengl::DrawManager::instance()->currentUserData()->getViewport();
            //Get and set the GLTransformInfo associated w/ this viewport and context
            scenegraph::GLTransformInfoPtr glTI =
                m_sceneGLTransformInfo->GetGLTransformInfo( viewport );
            const osg::Matrixd projectionMatrixOSG = glTI->GetProjectionMatrixOSG();

			int x = glTI->GetViewportOriginX();
			int y = glTI->GetViewportOriginY();
			int w = glTI->GetViewportWidth();
			int h = glTI->GetViewportHeight();
			osg::Viewport *vp = new osg::Viewport(x, y, w, h);
            
            
            lfx::core::PagingThread::instance()->setTransforms( projectionMatrixOSG, vp );
        }
        //ves::conductor::UIManager::instance()->AddUIToNode( camera );
        m_numContexts += 1;
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

    {
        vpr::Guard< vpr::Mutex > sv_guard( mValueLock );
        m_numContexts -= 1;
    }
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


    {
        vpr::Guard<vpr::Mutex> val_guard( mValueLock );
        newSceneViewer->init();
        newSceneViewer->setClearColor( osg::Vec4( 1.0, 0.0, 0.0, 0.0 ) );
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
        newSceneViewer->getCamera()->setDrawBuffer( GL_NONE );
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
    newSceneViewer->setViewport( 0, 0, 1, 1 );
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
////////////////////////////////////////////////////////////////////////////////
void App::bufferPreDraw()
{
#if 0
    glClearColor( 1.0, 0.0, 0.0, 0.0 );
    glClear( GL_COLOR_BUFFER_BIT );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void App::initScene()
{
    vprDEBUG( vesDBG, 0 ) << "|App::initScene" << std::endl << vprDEBUG_FLUSH;
    m_vrjHeadInterface.init( "VJHead" );
    m_startFrameInt = m_vrjHeadInterface->getTimeStamp();

    //Initialize all the XML objects
    XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new XMLCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new shader::ShaderCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "Model", new model::ModelCreator() );
    XMLObjectFactory::Instance()->RegisterObjectCreator( "CAD", new cad::CADCreator() );
    //Check and see if we have osg plugins available
    {
        const std::string osgExtension( "osg" );
        osgDB::ReaderWriter* osgReaderWriter =
            osgDB::Registry::instance()->getReaderWriterForExtension( osgExtension );
        osgDB::Registry::ReaderWriterList readerlist =
            osgDB::Registry::instance()->getReaderWriterList();
        if( ( readerlist.size() == 0 ) || !osgReaderWriter )
        {
            std::cerr <<
                      "OpenSceneGraph plugins are not available. Please make sure the OSG loaders are in your path."
                      << std::endl;
            exit();
        }
    }

    std::cout << std::endl;
    std::cout << "| ***************************************************************** |" << std::endl;
    //Need this loop here so manipulators know whether we are in desktop mode
    if( m_desktopMode )
    {
        ves::xplorer::scenegraph::SceneManager::instance()->SetDesktopMode( true );
    }

    //Set rtt mode for devices
    ves::xplorer::scenegraph::SceneManager::instance()->SetRTT( mRTT );
    //Define the rootNode, worldDCS, and lighting
    ves::xplorer::scenegraph::SceneManager::instance()->InitScene();
    ves::xplorer::scenegraph::SceneManager::instance()->ViewLogo( true );
    ves::xplorer::scenegraph::SceneManager::instance()->SetFrameStamp( mFrameStamp.get() );
    //Tell scenemanager if we are the master node
    ves::xplorer::scenegraph::SceneManager::instance()->SetMasterNode( m_isMaster );
    // Check commandline args for "--DesktopClusterControl"

    if( !m_desktopMode )
    {
        bool clusterControl = m_vm["DesktopClusterControl"].as<bool>();
        if( clusterControl )
        {
            ( *m_logStream ).information() << "Turning desktop cluster control on for the UI" << std::endl;
        }
        else
        {
            ( *m_logStream ).information() << "Desktop cluster control is off for the UI" << std::endl;
        }
        ves::xplorer::scenegraph::SceneManager::instance()->SetDesktopClusterControl( clusterControl );
    }
    // modelHandler stores the arrow and holds all data and geometry
    ModelHandler::instance()->InitScene();

    //Initialize DeviceHandler
    DeviceHandler::instance()->Initialize();
    //Tell the scenemanager about the devices
    SceneManager::instance()->SetDeviceHandlerGroup(
        DeviceHandler::instance()->GetDeviceGroup() );

    // navigation and cursor
    EnvironmentHandler::instance()->Initialize();
    /*if( m_desktopMode )
    {
        EnvironmentHandler::instance()->SetDesktopSize( m_screenWidth, m_screenHeight );
    }*/

    EnvironmentHandler::instance()->InitScene();

    // create steady state visualization objects
    SteadyStateVizHandler::instance()->InitScene();

    //create the volume viz handler
    _start_tick = _timer.tick();
    _tbvHandler = ves::xplorer::TextureBasedVizHandler::instance();
    _tbvHandler->SetMasterNode( m_isMaster );

    //Need to initialize this to ensure the model handling is correct.
    GraphicalPluginManager::instance()->Initialize();

    //Get or create UIManager
    ves::conductor::UIManager* m_UIManager = ves::conductor::UIManager::instance();

    //Hand current root node UIManager so it can create UI subgraph
    m_UIManager->Initialize( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void App::preFrame()
{
    VPR_PROFILE_GUARD_HISTORY( "App::preFrame", 20 );
    vprDEBUG( vesDBG, 3 ) << "|App::preFrame" << std::endl << vprDEBUG_FLUSH;

    {
        VPR_PROFILE_GUARD_HISTORY( "App::preFrame EnvironmentHandler", 20 );
        //Sets the worldDCS before it is synced
        EnvironmentHandler::instance()->PreFrameUpdate();
    }
#ifndef VES_QT_APP
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
    /*if( tempCommandPtr )
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
    }*/

    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame Framerate Calculations", 20 );
        m_currentFrameInt = m_vrjHeadInterface->getTimeStamp();
        double current_time = m_currentFrameInt.secd() - m_startFrameInt.secd();
        m_lastFrameInt = m_currentFrameInt;
        //This is order dependent
        //don't move above function call
        mFrameStamp->setFrameNumber( _frameNumber );
        mFrameStamp->setReferenceTime( current_time );

        double tempSimTime = mFrameStamp->getSimulationTime();
        mFrameDT = current_time - mLastFrameTime;
        mLastFrameTime = current_time;
        mFrameStamp->setSimulationTime( tempSimTime + mFrameDT );

        //This is a frame rate calculation
        double deltaTime = 0;
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
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame Pre Physics SceneManager", 20 );
        scenegraph::SceneManager::instance()->PrePhysicsLatePreFrameUpdate();
    }
    ///////////////////////
    {
        ;
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
        //LOG_INFO( "Physics dt " << mFrameDT );
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
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame UIManager", 20 );
        ves::conductor::UIManager::instance()->UpdateUIQuadPosition();
    }
    ///////////////////////
    {
        VPR_PROFILE_GUARD_HISTORY( "App::latePreFrame ModelHandler", 20 );
        ModelHandler::instance()->PreFrameUpdate();
    }
    ///////////////////////
    {
        //This call may need to be split similar to the SceneManager
        //Technically the navigation selection by the user should be processed
        //before the character or any of the physics processing is done
        //so that the position of the character reflects the current state of
        //all of the current user input. With the device processing here
        //the user input for nav with the character will lag 1 frame.
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
        _tbvHandler->SetCurrentTime( mLastFrameTime );
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
        eventmanager::EventMapper::instance()->LatePreFrameUpdate();
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
        mNavPosition.set(
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetMxCoreViewMatrix().getInverseMatrix().ptr() );
        //std::cout << mNavPosition << std::endl;
        //Need to figure out why osg::LightSource::ABSOLUTE_RF
        //does not work in a multi-context environment
        //jbkoch - Since in the CAVE each view is 90 degrees apart,
        //we do not want the light position relative to those matrix stacks,
        //we want it relative to the camera matrix
        //gmtl::Matrix44d tempNavMatrix =
        //    scenegraph::SceneManager::instance()->GetInvertedNavMatrix();
        gmtl::Vec4d tempVec( 0.0, -10000.0, 10000.0, 0.0 );
        tempVec = scenegraph::SceneManager::instance()->GetInvertedNavMatrix() * tempVec;
        osg::Vec4d position( tempVec[0], tempVec[1], tempVec[2], 0.0 );
        mSceneRenderToTexture->GetLight0()->setPosition( position );

        //Sneaky way to set uniform defined in
        //Access to these types of uniforms needs to be discussed with Paul
        if( !mRTT )
        {
            backdropFX::Manager::instance()->setLightPosition( 0, position );
        }
        /*gmtl::convertTo< double >(
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveNavSwitchNode()->GetMat() )*/;
    }
    ///////////////////////
    // Signal allowing other listeners to perform processing synced to draw
    {
        mLatePreFrame.signal();
    }
    ///////////////////////


#if !defined( VES_QT_APP )
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
    else
    {
        m_loadingUILock.release();
        vprDEBUG( vesDBG, 3 ) << "|\tApp::latePreFrame Try loading the UI."
                              << std::endl << vprDEBUG_FLUSH;
        m_loadingUILock.acquire();
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

    /*if( !m_windowIsOpen )
    {
        //Get the view matrix and the frustum from the draw manager
        vrj::opengl::DrawManager* glDrawManager =
        static_cast< vrj::opengl::DrawManager* >( getDrawManager() );
        const vrj::opengl::UserData* userData = glDrawManager->currentUserData();
        const vrj::opengl::WindowPtr window = userData->getGlWindow();
        m_windowIsOpen = window->isOpen();
    }*/

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

    EnvironmentHandler::instance()->PostFrameUpdate();

    ///update the transient frame number on the master
    _tbvHandler->UpdateTransientFrame();
    GraphicalPluginManager::instance()->PostFrameUpdate();

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

    if( !( *mViewportsChanged ) )
    {
        if( jccl::ConfigManager::instance()->isPendingStale() && m_windowIsOpen )
        {
            {
                vpr::Guard< vpr::Mutex > val_guard( mValueLock );
                mSceneRenderToTexture->InitScene( ( *sceneViewer )->getCamera() );
                m_numInitialized += 1;
                if( m_numInitialized == m_numContexts )
                {
                    m_render = true;
                }
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
        ( *sceneViewer )->setNearFarRatio( m_nearFarRatio );
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

    if( !m_render )
    {
        std::cout << "Cannot render yet." << std::endl;
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
    vrj::ProjectionPtr project = userData->getProjection();
    //const vrj::Frustum frustum = project->getFrustum();
    vrj::Frustum frustum = project->getFrustum();

    //Get the frustum values
    double l = 0.0;//frustum[ vrj::Frustum::VJ_LEFT ];
    double r = 0.0;//frustum[ vrj::Frustum::VJ_RIGHT ];
    double b = 0.0;//frustum[ vrj::Frustum::VJ_BOTTOM ];
    double t = 0.0;//frustum[ vrj::Frustum::VJ_TOP ];
    double n = 0.0;//frustum[ vrj::Frustum::VJ_NEAR ];
    double f = 0.0;//frustum[ vrj::Frustum::VJ_FAR ];

    //Get and set the GLTransformInfo associated w/ this viewport and context
    scenegraph::GLTransformInfoPtr glTI =
        m_sceneGLTransformInfo->GetGLTransformInfo( viewport );
    if( glTI )
    {
        // Compute location of left and right eyes
        const float positionScale = getDrawScaleFactor();

        float interocular_dist = viewport->getUser()->getInterocularDistance();
        interocular_dist *= positionScale; // Scale eye separation

        // Distance to move eye.
        const float eye_offset( interocular_dist * 0.5f );

        // NOTE: Eye coord system is -z forward, x-right, y-up
        ///We remove the translation component from the head matrix because
        ///the position is accounted for in the MxCore matrix so we need to
        ///remove it here. It gets multiplied in through the UpdateViewMatrix
        ///function call.
        if( project->getEye() == vrj::Projection::LEFT )
        {
            gmtl::Point3f eyePoint( -eye_offset, 0, 0 );
            const gmtl::Point3f left_eye_point = eyePoint;
            project->calcViewMatrix( gmtl::MAT_IDENTITY44F, left_eye_point,
                                     positionScale );
            const gmtl::Matrix44d viewMatrix =
                scenegraph::SceneManager::instance()->GetFullMatrix();
            eyePoint = gmtl::convertTo< float >( viewMatrix ) * eyePoint;
            const vrj::SurfaceProjectionPtr tempPtr =
                boost::dynamic_pointer_cast< vrj::SurfaceProjection >( project );
            if( tempPtr )
            {
                //frustum =
                //    m_sceneGLTransformInfo->CalculateFrustum( viewport, eyePoint );
            }
        }

        if( project->getEye() == vrj::Projection::RIGHT )
        {
            gmtl::Point3f eyePoint( eye_offset, 0, 0 );
            const gmtl::Point3f right_eye_point = eyePoint;
            project->calcViewMatrix( gmtl::MAT_IDENTITY44F, right_eye_point,
                                     positionScale );
            const gmtl::Matrix44d viewMatrix =
                scenegraph::SceneManager::instance()->GetFullMatrix();
            eyePoint = gmtl::convertTo< float >( viewMatrix ) * eyePoint;
            const vrj::SurfaceProjectionPtr tempPtr =
                boost::dynamic_pointer_cast< vrj::SurfaceProjection >( project );
            if( tempPtr )
            {
                //frustum =
                //    m_sceneGLTransformInfo->CalculateFrustum( viewport, eyePoint );
            }
        }

        //Get the view matrix from vrj
        const gmtl::Matrix44d vrjViewMatrix =
            gmtl::convertTo< double >( project->getViewMatrix() );

        //Multiply by the camera matrix (mNavPosition)
        glTI->UpdateViewMatrix( vrjViewMatrix, mNavPosition );
        const osg::Matrixd viewMatrixOSG = glTI->GetViewMatrixOSG();

        //Get the view matrix from a centered eye position
        m_sceneGLTransformInfo->CalculateCenterViewMatrix( project );

        //Now lets setup the frustum values for OSG
        //Get the frustum values
        l = frustum[ vrj::Frustum::VJ_LEFT ];
        r = frustum[ vrj::Frustum::VJ_RIGHT ];
        b = frustum[ vrj::Frustum::VJ_BOTTOM ];
        t = frustum[ vrj::Frustum::VJ_TOP ];
        n = frustum[ vrj::Frustum::VJ_NEAR ];
        f = frustum[ vrj::Frustum::VJ_FAR ];

        ///Remember that the VR Juggler near/far values are hard coded to
        ///0.1 and 10000. With OSG set to auto compute the near and far planes
        ///these values are overriden. If we use GLTransformInfo just after
        ///these values are set the projection matrix will not reflect
        ///what OSG is actually using for rendering.
        glTI->UpdateFrustumValues( l, r, b, t, n, f );
        //Get the projection matrix
        const osg::Matrixd projectionMatrixOSG = glTI->GetProjectionMatrixOSG();

        //Can't set viewport larger than fbo texture for rtt camera
        //We have to set the viewport for the frame buffer
        //Would like to inherit viewport from sv all way down but not possible
        //The rtt camera must be ABSOLUTE_RF because of implementation of
        //AutoTransform::computeLocalToWorldMatrix() and
        //AbsoluteModelTransform::computeLocalToWorldMatrix()
        //If it is not absolute, Physics and Manipulators will be broken
        sv->setProjectionMatrix( projectionMatrixOSG );
        // osg::Matrix::identity() ); //
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
        
        //Update the pageing thread
		osg::Vec3d eye, center, up;
		viewMatrixOSG.getLookAt( eye, center, up );
        lfx::core::PagingThread::instance()->setTransforms( osg::Vec3( eye ) );
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
        mSceneRenderToTexture->Update( mUpdateVisitor.get(), ves::open::xml::CommandPtr() );
    }
    // now force a recompute of the bounding volume while we are still in
    // the read/write app phase, this should prevent the need to recompute
    // the bounding volumes from within the cull traversal which may be
    // multi-threaded.
    getScene()->getBound();

    // Since the UI is directly under the root camera we need to manually update it
    //m_uiGroup->getBound();

#ifdef VE_SOUND
    m_listenerPosition.set( mNavPosition.getData() );
    osgAudio::SoundManager::instance()->setListenerMatrix( m_listenerPosition );
#endif
    vprDEBUG( vesDBG, 3 ) <<  "|\tEnd App LatePreframe Update"
                          << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void App::LoadUI()
{
    Poco::Logger& conductorLoggerInit = Poco::Logger::get( "conductor" );
    ves::xplorer::LogStreamPtr logStream;
    logStream = ves::xplorer::LogStreamPtr( new Poco::LogStream( conductorLoggerInit ) );
    ( *logStream ).information() << "Initialized conductor logger" << std::endl;
    //boost::ignore_unused_variable_warning( conductorLoggerInit );

    vprDEBUG( vesDBG, 2 ) << "|\tApp LoadUI" << std::endl << vprDEBUG_FLUSH;

    //Request connection to UIManager.EnterLeaveUI signal
    CONNECTSIGNAL_1( "UIManager.EnterLeaveUI", void( bool ), &App::UIEnterLeave,
                     m_connections, highest_Priority );

    {
        QStringList uiStyles = QStyleFactory::keys();
        for( size_t i = 0; i < uiStyles.size(); ++i )
        {
            ( *logStream ).critical() << "System styles " << uiStyles.at(i).toLocal8Bit().constData() << std::endl;
        }
    }

    // Create the Qt application event subsystem
    QApplication::setDesktopSettingsAware( true );
#if defined( VES_QT_APP )
    QApplication::setAttribute( Qt::AA_MacPluginApplication );
#endif
//#if !defined( VES_QT_APP )
    m_qtApp = new QApplication( argc, argv, 1 );
//#else
//    m_qtApp = new ves::xplorer::VESQtApplication( argc, argv, this );
//#endif

    {
        QString styleName = QApplication::style()->objectName();
        ( *logStream ).critical() << "Active UI style " << styleName.toLocal8Bit().constData() << std::endl;
    }

#ifdef VES_QT_RENDER_DEBUG
    QPushButton*  button = new QPushButton( "Test" );
    button->show();
    m_uiInitialized = true;
    return;
#endif

    // Equivalent to turning drag 'n' drop off for our application
    m_qtApp->setStartDragDistance( 10000 );

    //Get or create UIManager
    ves::conductor::UIManager* m_UIManager =
        ves::conductor::UIManager::instance();

    // Check commandline args for "--RegionDamaging"
    {
        bool regionDamage = m_vm["RegionDamaging"].as<bool>();
        if( regionDamage )
        {
            ( *logStream ).information() << "Turning region damaging on for the UI" << std::endl;
        }
        else
        {
            ( *logStream ).information() << "Region damaging is off for the UI" << std::endl;
        }
        m_UIManager->SetRegionDamaging( regionDamage );
    }

    // Wrap the widget in a UIElement
    ves::conductor::UIElementQt* element = new ves::conductor::UIElementQt();
    QWidget* mainUIWidget = new MainWindow( 0 );
    static_cast<ves::conductor::MainWindow*>( mainUIWidget )->SetLogSplitter( m_logSplitter );

    // Make UIManager's projection take up the entire viewable area of
    // the GL window
    // Now lets find out what we actually got back from the OS
    vrj::DisplayManager* displayManager = vrj::DisplayManager::instance();
    const std::vector< vrj::DisplayPtr >& displays =
        displayManager->getActiveDisplays();
    int originX = 0;
    int originY = 0;
    int width = 0;
    int height = 0;
    for( size_t i = 0; i < displays.size(); ++i )
    {
        vrj::DisplayPtr display = displays.at( i );
        display->getOriginAndSize( originX, originY, width, height );
    }
    ( *logStream ).information() << "|\tWindow value: " << width << " "
              << height << std::endl << std::flush;

    if( !ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() &&
       !ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopClusterControl() )
    {
        //These values must match the values in UIElement on line 105
        width = 600;
        height = 967;
    }
    //Only the height value is used to enable flipping the y value to put
    //it in qt space.
    m_UIManager->SetRectangle( 0, width, 0, height );

    element->SetInitialImageWidthAndHeight( 600, height );
    element->SetWidget( mainUIWidget );

    m_UIManager->AddElement( element );

    //In desktop mode start the main UI minimized
    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        m_UIManager->MinimizeElement( element );
    }
    else
    {
        //If we are in cluster mode and we have asked for desktop control of the cluster
        if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopClusterControl() )
        {
            //If we are a render node then make the UI invisible
            if( !ves::xplorer::scenegraph::SceneManager::instance()->IsMasterNode() )
            {
                //In cluster mode set all of the render node UIs to fully transparent
                m_UIManager->SetOpacity( m_vm["UIOpacity"].as< double >() );
            }
        }
    }

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
#if defined( VES_QT_APP )
    while( !jccl::ConfigManager::instance()->isPendingStale() )
    {
        vpr::System::msleep( 200 );
    }
    m_loadingUILock.acquire();
#endif
    LoadUI();
#if defined( VES_QT_APP )
    m_loadingUILock.release();
#endif
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
#if defined( VES_QT_APP )
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
////////////////////////////////////////////////////////////////////////////////
void App::SetDesktopInfo( bool mode, int screenWidth, int screenHeight )
{
    m_desktopMode = mode;
    m_screenWidth = screenWidth;
    m_screenHeight = screenHeight;
}
#if defined( VES_QT_APP )
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
