/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>

#include <switchwire/EventManager.h>
#include <switchwire/OptionalMacros.h>

#ifdef VE_SOUND
#include <ves/xplorer/scenegraph/Sound.h>
#endif

#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>
#include <osg/io_utils>

#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/WriteFile>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

// --- BackdropFX Includes --- //
#include <backdropFX/Version.h>
#include <backdropFX/Manager.h>
#include <backdropFX/DepthPartition.h>
#include <backdropFX/ShaderModule.h>
#include <backdropFX/ShaderModuleVisitor.h>
#include <backdropFX/ShaderModuleUtils.h>

#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAudio/SoundManager.h>
#include <osgAudio/SoundRoot.h>
#include <osgAudio/SoundNode.h>
#include <osgAudio/SoundState.h>
#   if defined( ENABLE_SUBSYSTEM_FMOD ) && defined( VPR_OS_Linux )
#   include <fmod.hpp>
#   include <osgAudio/AudioEnvironment.h>
#   endif
#endif //VE_SOUND

// --- VR Juggler Includes --- //
#include <jccl/RTRC/ConfigManager.h>

#include <gmtl/gmtl.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- osgWorks Includes --- //
#include <osgwMx/MxGamePad.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <string>
#include <istream>
#include <sstream>

using namespace ves::xplorer::scenegraph;

vprSingletonImpLifetime( SceneManager, 1 );

////////////////////////////////////////////////////////////////////////////////
SceneManager::SceneManager()
    :
    mRootNode( NULL ),
    mModelRoot( NULL ),
    m_graphicalPluginManager( NULL ),
    m_cameraManager( NULL ),
    m_highlightManager( NULL ),
    m_manipulatorManager( NULL ),
    mLogoNode( NULL ),
    mLogoSwitch( NULL ),
    mNavSwitch( NULL ),
    mActiveNavDCS( NULL ),
    m_navDCS( NULL ),
    mNetworkDCS( NULL ),
#ifdef VE_SOUND
    m_sound( NULL ),
#endif
    m_clearColorUniform(
        new osg::Uniform( "clearColor", osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) ) ),
    mFrameStamp( NULL ),
    mCharacterController( NULL ),
    m_isRTTOn( false ),
    m_isDesktopMode( false ),
    m_screenAlignedNormals( true ),
    m_isMasterNode( true ),
    m_previousTime( 0 ),
    m_deltaTime( 0 ),
    m_viewMatrix( new osgwMx::MxCore() ),
    m_userHeight( 5.0 ),
    m_isDesktopClusterControl( true )
{
    gmtl::Vec3d x_axis( 1.0, 0.0, 0.0 );
    m_zUpTransform = gmtl::makeRot< gmtl::Matrix44d >( gmtl::AxisAngled( gmtl::Math::deg2Rad( 90.0 ), x_axis ) );
    m_defaultView = gmtl::makeRot< gmtl::Matrix44d >( gmtl::AxisAngled( gmtl::Math::deg2Rad( 270.0 ), x_axis ) );

    // Set some MxCore defaults:

    m_viewMatrix->setInitialValues(
        osg::Vec3d( 0., 0., 1. ),
        osg::Vec3d( 0., 1., 0. ),
        osg::Vec3d( 0., 0., 0. ) );
    m_viewMatrix->setRotateScale( 0.75 );
    m_viewMatrix->setOrbitCenterPoint( osg::Vec3d( 0., 10., 0. ) );
    m_viewMatrix->reset();

    ///Setup some default uniforms so that they are not replicated all over the
    ///code base
    ///For models with textures
    const std::string shaderName = osgDB::findDataFile( "null_glow_texture.fs" );
    osg::ref_ptr< osg::Shader > fragShader =
        osg::Shader::readShaderFile( osg::Shader::FRAGMENT, shaderName );

    m_nullGlowTextureProgram = new osg::Program();
    m_nullGlowTextureProgram->addShader( fragShader.get() );

    m_nullGlowTextureUniform = new osg::Uniform( "tex", 0 );
    ///For models without textures

    switchwire::EventManager::instance()->RegisterSignal(
        ( &m_updateData ),
        "SceneManager.UpdateData" );

    CONNECTSIGNAL_4( "GameController.SelectionButtonPress", void( gadget::Keys, int, int, int ),
                     &SceneManager::ShowSelectionLine,
                     m_connections, normal_Priority );

    CONNECTSIGNAL_4( "GameController.SelectionButtonRelease", void( gadget::Keys, int, int, int ),
                     &SceneManager::HideSelectionLine,
                     m_connections, normal_Priority );

    CONNECTSIGNAL_4( "Wand.ButtonPress0", void( gadget::Keys, int, int, int ),
                     &SceneManager::ShowSelectionLine,
                     m_connections, normal_Priority );

    CONNECTSIGNAL_4( "Wand.ButtonRelease0", void( gadget::Keys, int, int, int ),
                     &SceneManager::HideSelectionLine,
                     m_connections, normal_Priority );

    CONNECTSIGNAL_2( "GameController.StartEndPoint", void( osg::Vec3d, osg::Vec3d ),
                     &SceneManager::SetSelectionLineStartEndPoint,
                     m_connections, normal_Priority );

    CONNECTSIGNAL_2( "Wand.StartEndPoint", void( osg::Vec3d, osg::Vec3d ),
                     &SceneManager::SetSelectionLineStartEndPoint,
                     m_connections, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::Initialize()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SceneManager::~SceneManager()
{
#ifdef VE_SOUND
    /*    try
        {
            osgAudio::SoundManager::instance()->shutdown();
        }
        catch( ... )
        {
            std::cerr << "|\tosgAL::SoundManager is unable to shutdown."
                << std::endl;
        }*/
#endif

#ifdef VE_SOUND
    //delete m_sound;
#endif
}
////////////////////////////////////////////////////////////////////////////////
#if defined( VE_SOUND ) && defined( ENABLE_SUBSYSTEM_FMOD ) && defined( VPR_OS_Linux )
///Configure the System interface of FMOD
void SceneManager::ConfigureFMODSystem( FMOD::System* system )
{
    //The channel count that is overwritten for each speaker mode is as follows:
    //FMOD_SPEAKERMODE_RAW - Channel count is unaffected.
    //FMOD_SPEAKERMODE_MONO - Channel count is set to 1.
    //FMOD_SPEAKERMODE_STEREO - Channel count is set to 2.
    //FMOD_SPEAKERMODE_QUAD - Channel count is set to 4.
    //FMOD_SPEAKERMODE_SURROUND - Channel count is set to 5.
    //FMOD_SPEAKERMODE_5POINT1 - Channel count is set to 6.
    //FMOD_SPEAKERMODE_7POINT1 - Channel count is set to 8.
    //FMOD_SPEAKERMODE_PROLOGIC - Channel count is set to 2
    FMOD_RESULT result;
    result = system->setOutput( FMOD_OUTPUTTYPE_ALSA );
    result = system->setSpeakerMode( FMOD_SPEAKERMODE_5POINT1 );
}
#endif
////////////////////////////////////////////////////////////////////////////////
void SceneManager::InitScene()
{
    std::cout <<
              "|  Initializing....................................... SceneManager |"
              << std::endl;

    m_vrjHead.init( "VJHead" );
    m_vrjHeadMatrix =
        gmtl::convertTo< double >( m_vrjHead->getData() );
    m_vrjHeadMatrix = m_zUpTransform * m_vrjHeadMatrix * m_defaultView;
    m_previousVRJHeadMatrix = m_vrjHeadMatrix;

    osg::Vec3d up, dir, pos;
    double fovy;
    m_viewMatrix->getInitialValues( up, dir, pos, fovy );
    m_lastHeadLocation = gmtl::makeTrans< gmtl::Point3d >( m_vrjHeadMatrix );
    pos[ 0 ] = m_vrjHeadMatrix.mData[ 12 ];
    pos[ 1 ] = m_vrjHeadMatrix.mData[ 13 ];
    pos[ 2 ] = m_vrjHeadMatrix.mData[ 14 ];
    m_viewMatrix->setInitialValues( up, dir, pos, fovy );

    //mRootNode = new ves::xplorer::scenegraph::Group();
    if( !mRootNode.valid() )
    {
        std::cout << " big problems " << std::endl;
        exit( 1 );
    }
    mRootNode->setName( "Root Node" );
    mRootNode->setThreadSafeRefUnref( true );

    mModelRoot = new osg::Group();
    mModelRoot->setName( "Model Root Node" );

    m_graphicalPluginManager = new Group();
    m_graphicalPluginManager->setName( "Graphical Plugin Manager" );
    mModelRoot->addChild( m_graphicalPluginManager.get() );

    m_cameraManager = new camera::CameraManager();
    m_cameraManager->setName( "Camera Manager" );
    mModelRoot->addChild( m_cameraManager.get() );

    m_highlightManager = new highlight::HighlightManager();
    m_highlightManager->setName( "Highlight Manager" );
    mModelRoot->addChild( m_highlightManager.get() );

    m_manipulatorManager = new manipulator::ManipulatorManager();
    m_manipulatorManager->setName( "Manipulator Manager" );
    mModelRoot->addChild( m_manipulatorManager.get() );

#ifdef VE_SOUND
    try
    {
#if defined( ENABLE_SUBSYSTEM_FMOD ) && defined( VPR_OS_Linux )
        osgAudio::AudioEnvironment::instance()->
        setPreSystemInitFunc( &SceneManager::ConfigureFMODSystem );
#endif
        osgAudio::SoundManager::instance()->init( 16, true );
        osgAudio::SoundManager::instance()->getEnvironment()->
        setDistanceModel( osgAudio::InverseDistance );
        osgAudio::SoundManager::instance()->getEnvironment()->
        setDopplerFactor( 1 );
        /*
        #ifdef ENABLE_SUBSYSTEM_FMOD
                std::vector< std::string > driverNames;
                std::vector< std::string > driverPriorities;
                driverPriorities.push_back( "surround71" );
                driverPriorities.push_back( "surround51" );
                driverPriorities.push_back( "surround50" );
                driverPriorities.push_back( "surround41" );
                driverPriorities.push_back( "surround40" );
                driverPriorities.push_back( "default" );
                ///driverPriorities.push_back( "iec958" );
                for( size_t i = 0; i < driverPriorities.size(); ++i )
                {
                    std::vector::const_iterator iter =
                        std::find( driverNames.begin(),
                                    driverNames.end(),
                                    driverPriorities.at( i ) );
                    if( iter != driverNames.end() )
                    {
                        size_t driverNumber = iter - driverNames.begin();
                        result = fmodSystem->setDriver( driverNumber );
                        if( result == FMOD_OK )
                        {
                            break;
                        }
                    }
                }
                //Setup the driver to use on linux with fmod
                FMOD::System* fmodSystem =
                    osgAudio::SoundManager::instance()->getEnvironment()->getSystem();
                int numDrivers;
                FMOD_RESULT result;
                result = fmodSystem->getNumDrivers( &numDrivers );
                if( numDrivers > 0 )
                {
                    std::cout << "Initializing FMOD. Detected "
                        << numDrivers << " drivers." << std::endl;
                    numDrivers -= 1;
                    for( int idx = numDrivers; idx >= 0; --idx )
                    {
                        const int nameLen( 128 );
                        char name[ nameLen - 1 ];
                        FMOD_GUID guid;
                        result = fmodSystem->getDriverInfo( idx, name, nameLen, &guid );
                        std::string driverName( name );
                        std::cout << idx << ": " << name << std::endl;
                        if( driverName == "surround71" )
                        {
                            result = fmodSystem->setDriver( idx );
                            if( result == FMOD_OK )
                            {
                                break;
                            }
                        }
                        else if( driverName == "surround51" )
                        {
                            result = fmodSystem->setDriver( idx );
                            if( result == FMOD_OK )
                            {
                                break;
                            }
                        }
                        else if( driverName == "surround50" )
                        {
                            result = fmodSystem->setDriver( idx );
                            if( result == FMOD_OK )
                            {
                                break;
                            }
                        }
                        else if( driverName == "surround40" )
                        {
                            result = fmodSystem->setDriver( idx );
                            if( result == FMOD_OK )
                            {
                                break;
                            }
                        }
                        else if( driverName == "surround41" )
                        {
                            result = fmodSystem->setDriver( idx );
                            if( result == FMOD_OK )
                            {
                                break;
                            }
                        }
                        else if( driverName == "default" )
                        {
                            result = fmodSystem->setDriver( idx );
                            if( result == FMOD_OK )
                            {
                                break;
                            }
                        }
                        //else if( driverName == "iec958" )
                        //{
                        //    result = fmodSystem->setDriver( idx );
                        //    if( result == FMOD_OK )
                        //    {
                        //        break;
                        //    }
                        //}
                        else
                        {
                            result = fmodSystem->setDriver( 0 );
                            if( result == FMOD_OK )
                            {
                                break;
                            }
                        }
                    }
                }
        #endif
        */
        osg::ref_ptr< osgAudio::SoundRoot > soundRoot =
            new osgAudio::SoundRoot();
        soundRoot->setName( "Sound Root" );
        mRootNode->addChild( soundRoot.get() );
    }
    catch( std::exception& ex )
    {
        std::cerr << "|\tosgAudio::SoundManager is unable to initialize: "
                  << ex.what() << std::endl;
    }
#endif
    mNavSwitch = new osg::Switch();
    mNavSwitch->setName( "Nav Switch" );

    m_navDCS = new ves::xplorer::scenegraph::DCS();
    m_navDCS->SetName( "World DCS" );
    //Setup world nav switch
    mNavSwitch->addChild( m_navDCS.get() );
    //Set an initial -90 rotation to put the nav in Z up land
    m_navDCS->SetMat( m_defaultView );

    //Setup logo nav switch
    mNavSwitch->addChild( new ves::xplorer::scenegraph::DCS() );

    //Setup network nav switch
    mNavSwitch->addChild( new ves::xplorer::scenegraph::DCS() );

    //Create the switch for our logo
    _createLogo();

    mNetworkDCS  = new osg::Group();
    mNetworkDCS->setName( "Network DCS" );

    mLogoSwitch->addChild( mModelRoot.get() );
    mLogoSwitch->addChild( mLogoNode.get() );
    mLogoSwitch->addChild( mNetworkDCS.get() );

    mRootNode->addChild( mLogoSwitch.get() );
    //Add the m_navDCS here because the nav matrix is pulled out
    //App.cxx and applied to the view matrix
    mRootNode->addChild( mNavSwitch.get() );
    ///Try to load the osgPT Polytans plugin to load all
    ///supported PolyTrans file types
#ifdef _DEBUG
    const std::string pluginName( "osgdb_PolyTransd.dll" );
#else
    const std::string pluginName( "osgdb_PolyTrans.dll" );
#endif //_DEBUG
    bool loadedLib = osgDB::Registry::instance()->loadLibrary( pluginName );
    if( !loadedLib )
    {
        vprDEBUG( vesDBG, 2 ) << "Can't load plugin \""
                              << pluginName << "\"." << std::endl << vprDEBUG_FLUSH;
    }

    SetActiveSwitchNode( 1 );

    //Create the character controller
    mCharacterController = new CharacterController();
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetRootNode( osg::Group* rootNode )
{
    mRootNode = rootNode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* SceneManager::GetRootNode() const
{
    return mRootNode.get();
}
////////////////////////////////////////////////////////////////////////////////
GLTransformInfoPtr const SceneManager::GetGLTransformInfo(
    vrj::ViewportPtr const viewport )
{
    GLTransformInfoMap::const_iterator itr =
        m_glTransformInfoMap.find( viewport );
    if( itr != m_glTransformInfoMap.end() )
    {
        return itr->second;
    }

    std::cout << "SceneManager::GetGLTransformInfo - "
              << "GLTransformInfo not found!" << std::endl;

    return GLTransformInfoPtr();
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* SceneManager::GetModelRoot() const
{
    return mModelRoot.get();
}
////////////////////////////////////////////////////////////////////////////////
Group& SceneManager::GetGraphicalPluginManager() const
{
    return *m_graphicalPluginManager.get();
}
////////////////////////////////////////////////////////////////////////////////
camera::CameraManager& SceneManager::GetCameraManager() const
{
    return *m_cameraManager.get();
}
////////////////////////////////////////////////////////////////////////////////
highlight::HighlightManager& SceneManager::GetHighlightManager() const
{
    return *m_highlightManager.get();
}
////////////////////////////////////////////////////////////////////////////////
manipulator::ManipulatorManager& SceneManager::GetManipulatorManager() const
{
    return *m_manipulatorManager.get();
}
////////////////////////////////////////////////////////////////////////////////
DCS* SceneManager::GetNavDCS() const
{
    return m_navDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneManager::GetInvertedNavMatrix() const
{
    return m_invertedNavMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& SceneManager::GetInvertedNavMatrixOSG() const
{
    return m_invertedNavMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneManager::GetHeadMatrix() const
{
    return m_vrjHeadMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneManager::GetGlobalViewMatrix() const
{
    return m_globalViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& SceneManager::GetGlobalViewMatrixOSG() const
{
    return m_globalViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneManager::GetInvertedGlobalViewMatrix() const
{
    return m_invertedGlobalViewMatrix;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Matrixd& SceneManager::GetInvertedGlobalViewMatrixOSG() const
{
    return m_invertedGlobalViewMatrixOSG;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneManager::GetPureNavMatrix() const
{
    return m_pureNav;
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneManager::GetFullMatrix() const
{
    return m_pureFull;
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* SceneManager::GetNetworkDCS() const
{
    return mNetworkDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
osgwMx::MxCore& SceneManager::GetMxCoreViewMatrix() const
{
    return *m_viewMatrix.get();
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::ViewLogo( bool trueFalse )
{
    if( trueFalse )
    {
        SetActiveSwitchNode( 1 );
    }
    else
    {
        SetActiveSwitchNode( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::_createLogo()
{
    if( !mLogoSwitch )
    {
        mLogoSwitch = new osg::Switch();
        mLogoSwitch->setName( "Models Graph" );
    }

    if( mLogoNode.valid() )
    {
        return;
    }

    mLogoNode = new ves::xplorer::scenegraph::DCS();
    mLogoNode->setName( "Logo Node" );

    if( IsDesktopMode() )
    {
        mLogoNode->setPosition( osg::Vec3d( -1.7, 10.0, 0.0 ) );
    }
    else
    {
        mLogoNode->setPosition( osg::Vec3d( -1.7, 10.0, 6.0 ) );
    }

    mLogoNode->setAttitude(
        osg::Quat( osg::DegreesToRadians( -45.0 ),
                   osg::Vec3d( 0.0, 0.0, 1.0 ) ) );

    mLogoNode->setScale( osg::Vec3d( 0.01, 0.01, 0.01 ) );

    //Add the logo model
    osg::ref_ptr< osg::Node > vesuiteNode =
        osgDB::readNodeFile( "logo/ve-suite.ive" );
    //I think that the logo node is added before we do all of the bdfx
    //initialization so there is no need to run rsm at this stage.
    /*if( !m_isRTTOn )
    {
        backdropFX::ShaderModuleCullCallback::ShaderMap tempMap;
        ves::xplorer::scenegraph::FindParentsVisitor parentVisitor( mLogoNode.get(), backdropFX::Manager::instance()->getManagedRoot() );
        osg::NodePath nodePath = parentVisitor.GetParentNodePath();
        osg::StateSet* tempState = backdropFX::accumulateStateSetsAndShaderModules( tempMap, nodePath );

        backdropFX::ShaderModuleVisitor smv;
        //smv.setRemoveFFPState(false);
        smv.setSupportSunLighting( false ); // Use shaders that support Sun lighting.
        smv.setInitialStateSet( tempState, tempMap );

        backdropFX::convertFFPToShaderModules( vesuiteNode.get(), &smv );
    }*/
    mLogoNode->addChild( vesuiteNode.get() );
    /*if( !m_isRTTOn )
    {
        backdropFX::RebuildShaderModules rsm;
        backdropFX::Manager::instance()->getManagedRoot()->accept( rsm );
    }*/
    /*
    std::string vsName = osgDB::findDataFile( "phong.vs" );
    std::string fsName = osgDB::findDataFile( "phong.fs" );
    osg::ref_ptr< osg::Shader > vs =
        osg::Shader::readShaderFile( osg::Shader::VERTEX, vsName );
    osg::ref_ptr< osg::Shader > fs =
        osg::Shader::readShaderFile( osg::Shader::FRAGMENT, fsName );
    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( vs.get() );
    program->addShader( fs.get() );

    osg::ref_ptr< osg::StateSet > stateset = vesuiteNode->getOrCreateStateSet();
    stateset->setAttribute(
        program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    */
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetActiveSwitchNode( int activeNode )
{
    mLogoSwitch->setSingleChildOn( activeNode );
    mNavSwitch->setSingleChildOn( activeNode );
    mActiveNavDCS = GetActiveNavSwitchNode();
    ///Now reset the dcs back to its former position so that the nav
    ///information is defined on a per node basis.
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::PrePhysicsLatePreFrameUpdate()
{
    m_vrjHeadMatrix = gmtl::convertTo< double >( m_vrjHead->getData() );
    ///Convert the head matrix to Z up land and then back out purely the
    ///rotation component to get a pure matrix with head rotation and
    ///position in Z up land.
    m_vrjHeadMatrix = m_zUpTransform * m_vrjHeadMatrix * m_defaultView;

    //If we do not constantly send this signal when we are using a tracked
    //head position the character controller will become out of sync with
    //the view position and the head constants. The update signal forces
    //the character controller to update the character position based
    //on the current view matrix.
    if( !m_isDesktopMode && mCharacterController->IsEnabled() )
    {
        m_updateData.signal( true );
    }

    m_previousVRJHeadMatrix = m_vrjHeadMatrix;

    ///If the logo dcs is active no nav is allowed
    if( mNavSwitch->getValue( 1 ) )
    {
        osg::Vec3d up, dir, pos;
        double fovy;
        m_viewMatrix->getInitialValues( up, dir, pos, fovy );
        ///We grab the head location here so that we have a base location for
        ///where the head is in the scene
        m_lastHeadLocation = gmtl::makeTrans< gmtl::Point3d >( m_vrjHeadMatrix );
        pos[ 0 ] = m_vrjHeadMatrix.mData[ 12 ];
        pos[ 1 ] = m_vrjHeadMatrix.mData[ 13 ];
        pos[ 2 ] = m_vrjHeadMatrix.mData[ 14 ];
        m_viewMatrix->setInitialValues( up, dir, pos, fovy );
        m_viewMatrix->reset();
    }

    ///Get the tracked head location for and create a delta that can be added
    ///to the current matrix stack in MxCore. This delta is in Z up land and
    ///needs to be a delta so that it is an adder to whatever is being done
    ///by the user through input devices.
    gmtl::Point3d headLocation =
        gmtl::makeTrans< gmtl::Point3d >( m_vrjHeadMatrix );
    m_deltaHeadLocation = headLocation - m_lastHeadLocation;

    osg::Vec3d deltaHeadPosition( m_deltaHeadLocation.mData[ 0 ],
                                  m_deltaHeadLocation.mData[ 1 ],
                                  m_deltaHeadLocation.mData[ 2 ] );

    // extract the orientation component of the current view matrix
    osg::Matrixd viewOrientationMatrix =  m_viewMatrix->getOrientationMatrix();

    // the viewOrientationMatrix will have the results of any nav rotations done with the
    // joystick/gamepad "baked" into it. Since navigation can change the reference coordinate
    // frame, we'll need to apply the inverse rotation to the head tracker position delta to
    // keep things consistent. Head tracker positions are in "Z-up" space, so we also need
    // to apply the same Z-up transformation to the inverted view orientation matrix.
    osg::Matrixd inverseViewOrientationMatrix = osg::Matrixd::inverse( viewOrientationMatrix )
                                                * osg::Matrixd( m_zUpTransform.getData() );

    // we can think of deltaHeadPosition as a "direction vector" whose magnitude is the amount
    // the head has moved since the last frame. Before adding the delta to our view matrix, we
    // need to apply the counter-rotation we calculated above to it.
    osg::Vec3d rotatedDeltaHeadPosition = inverseViewOrientationMatrix * deltaHeadPosition;

    m_viewMatrix->setPosition( m_viewMatrix->getPosition() + rotatedDeltaHeadPosition );

    ///This is the distance from the VR Juggler defined ground plane to the
    ///users head so we do not care what coordinate system they are relative to.
    m_userHeight = headLocation.mData[ 2 ];
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::LatePreFrameUpdate()
{
    //Check to see if the character has run into any static objects
    //If the character is not enabled or is not in contact with static objects
    //then the view position constants will be updated.
    //if( !CheckCharacterCollisionState() )
    {
        UpdateHeadPositionConstants();
    }

    gmtl::identity( m_invertedNavMatrix );

    //Create the OpenGL oriented matrices used in the frustum calculations so
    //that the character, physics and the tracked head position will render
    //properly.
    m_pureNav.set( m_viewMatrix->getMatrix().ptr() );
    m_pureNav = m_defaultView * m_pureNav;
    m_pureFull = m_pureNav;

    m_pureNav.mData[ 12 ] = m_pureNav.mData[ 12 ] - m_lastValidVRJHeadLocation.mData[ 0 ];
    m_pureNav.mData[ 13 ] = m_pureNav.mData[ 13 ] - m_lastValidVRJHeadLocation.mData[ 1 ];
    m_pureNav.mData[ 14 ] = m_pureNav.mData[ 14 ] - m_lastValidVRJHeadLocation.mData[ 2 ];

    gmtl::Matrix44d navMatrix;
    navMatrix.set( m_viewMatrix->getInverseMatrix().ptr() );
    navMatrix = m_zUpTransform * navMatrix;

    ///Take the VR Juggler head position out of the view matrix
    ///so that we have pure nav data.
    navMatrix.mData[ 12 ] = navMatrix.mData[ 12 ] + m_lastValidHeadLocation.mData[ 0 ];
    navMatrix.mData[ 13 ] = navMatrix.mData[ 13 ] + m_lastValidHeadLocation.mData[ 1 ];
    navMatrix.mData[ 14 ] = navMatrix.mData[ 14 ] + m_lastValidHeadLocation.mData[ 2 ];
    //navMatrix.mState = gmtl::Matrix44d::FULL;

    gmtl::invert( m_invertedNavMatrix, navMatrix );

    m_invertedNavMatrixOSG.set( m_invertedNavMatrix.getData() );

    ///We need to remove the position from the head matrix because it is
    ///already being accounted for in the navMatrix for MxCore. The
    ///m_globalViewMatrix is the matrix that we need to use throughout ves
    ///for the view matrix. It includes everything except for the final
    ///transformation for a given projection for a given context and viewport.
    gmtl::Matrix44d headRotMat = m_vrjHeadMatrix;
    //headRotMat.mData[ 12 ] = 0.0;
    //headRotMat.mData[ 13 ] = 0.0;
    //headRotMat.mData[ 14 ] = 0.0;

    gmtl::identity( m_globalViewMatrix );
    m_globalViewMatrix = m_invertedNavMatrix * headRotMat;
    m_globalViewMatrixOSG.set( m_globalViewMatrix.getData() );

    gmtl::identity( m_invertedGlobalViewMatrix );
    gmtl::invert( m_invertedGlobalViewMatrix, m_globalViewMatrix );
    m_invertedGlobalViewMatrixOSG.set( m_globalViewMatrix.getData() );

    m_deltaTime = mFrameStamp->getSimulationTime() - m_previousTime;
    m_previousTime = mFrameStamp->getSimulationTime();

    m_cameraManager->LatePreFrameUpdate();
}
////////////////////////////////////////////////////////////////////////////////
bool SceneManager::CheckCharacterCollisionState() const
{
    if( !m_isDesktopMode )
    {
        //gmtl::Vec3d deltaVec = m_deltaHeadLocation;
        //double changeInHeadPosition = gmtl::length( deltaVec );

        if( mCharacterController->IsEnabled() )
        {
            if( mCharacterController->isColliding() )
            {
                //std::cout << " is colliding " << changeInHeadPosition <<std::endl;
                return true;
            }
            //std::cout << " not colliding " << changeInHeadPosition << std::endl;
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::UpdateHeadPositionConstants()
{
    m_lastHeadLocation =
        gmtl::makeTrans< gmtl::Point3d >( m_vrjHeadMatrix );
    m_lastValidHeadLocation = m_lastHeadLocation;
    m_lastValidVRJHeadLocation =
        gmtl::makeTrans< gmtl::Point3d >( gmtl::convertTo< double >( m_vrjHead->getData() ) );
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::PostFrameUpdate()
{
    m_cameraManager->PostFrameUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::PushBackGLTransformInfo(
    vrj::ViewportPtr viewport,
    GLTransformInfoPtr glTransformInfo )
{
    m_glTransformInfoMap[ viewport ] = glTransformInfo;
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* SceneManager::GetActiveSwitchNode() const
{
    osg::Switch::ValueList boolList = mLogoSwitch->getValueList();

    for( size_t i = 0; i < boolList.size(); ++i )
    {
        if( boolList.at( i ) )
        {
            return static_cast< osg::Group* >( mLogoSwitch->getChild( i ) );
        }
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
DCS* SceneManager::GetActiveNavSwitchNode() const
{
    osg::Switch::ValueList boolList = mNavSwitch->getValueList();

    for( size_t i = 0; i < boolList.size(); ++i )
    {
        if( boolList.at( i ) )
        {
            return static_cast< ves::xplorer::scenegraph::DCS* >(
                       mNavSwitch->getChild( i ) );
        }
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetBackgroundColor( std::vector< double > color )
{
    osg::Vec4 clearColor( color.at( 0 ), color.at( 1 ), color.at( 2 ), 1.0 );
    if( m_isRTTOn )
    {
        m_clearColorUniform->set( clearColor );
    }
    else
    {
        backdropFX::DepthPartition& dp =
            backdropFX::Manager::instance()->getDepthPartition();
        dp.setClearColor( clearColor );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::Shutdown()
{
#ifdef VE_SOUND
    osgAudio::SoundManager::instance()->shutdown();
#endif
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetFrameStamp( osg::FrameStamp* frameStamp )
{
    mFrameStamp = frameStamp;
}
////////////////////////////////////////////////////////////////////////////////
osg::FrameStamp* SceneManager::GetFrameStamp() const
{
    return mFrameStamp.get();
}
////////////////////////////////////////////////////////////////////////////////
CharacterController& SceneManager::GetCharacterController() const
{
    return *mCharacterController;
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform& SceneManager::GetClearColorUniform() const
{
    return *m_clearColorUniform.get();
}
////////////////////////////////////////////////////////////////////////////////
bool SceneManager::IsRTTOn()
{
    return m_isRTTOn;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetRTT( bool isRTTOn )
{
    m_isRTTOn = isRTTOn;
}
////////////////////////////////////////////////////////////////////////////////
bool SceneManager::IsDesktopMode() const
{
    return m_isDesktopMode;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetDesktopMode( bool isDesktopMode )
{
    m_isDesktopMode = isDesktopMode;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetScreenAlignedNormals( bool isScreenAligned )
{
    m_screenAlignedNormals = isScreenAligned;
}
////////////////////////////////////////////////////////////////////////////////
bool SceneManager::IsScreenAligned()
{
    return m_screenAlignedNormals;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetMasterNode( bool isMasterNode )
{
    m_isMasterNode = isMasterNode;
}
////////////////////////////////////////////////////////////////////////////////
bool SceneManager::IsMasterNode()
{
    return m_isMasterNode;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetDesktopClusterControl( bool control )
{
    m_isDesktopClusterControl = control;
}
////////////////////////////////////////////////////////////////////////////////
bool SceneManager::IsDesktopClusterControl() const
{
    return m_isDesktopClusterControl;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetDeviceHandlerGroup( osg::Group* deviceGroup )
{
    m_deviceHandlerGroup = deviceGroup;
}
////////////////////////////////////////////////////////////////////////////////
osg::Group& SceneManager::GetDeviceHandlerGroup()
{
    return *m_deviceHandlerGroup.get();
}
////////////////////////////////////////////////////////////////////////////////
GLTransformInfoPtr const SceneManager::GetCurrentGLTransformInfo()
{
    return m_currentGLTransformInfo;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetCurrentGLTransformInfo( GLTransformInfoPtr const transformInfo )
{
    m_currentGLTransformInfo = transformInfo;
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Point3d& SceneManager::GetCenterPoint()
{
    return m_centerPoint;
}
////////////////////////////////////////////////////////////////////////////////
double SceneManager::GetDeltaFrameTime()
{
    return m_deltaTime;
}
////////////////////////////////////////////////////////////////////////////////
osg::Program* SceneManager::GetNullGlowTextureProgram()
{
    return m_nullGlowTextureProgram.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* SceneManager::GetNullGlowTextureUniform()
{
    return m_nullGlowTextureUniform.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Program* SceneManager::GetNullGlowProgram()
{
    return m_nullGlowProgram.get();
}
////////////////////////////////////////////////////////////////////////////////
double const& SceneManager::GetUserHeight() const
{
    return m_userHeight;
}
////////////////////////////////////////////////////////////////////////////////
double SceneManager::GetCurrentTime() const
{
    return m_vrjHead->getTimeStamp().secd();
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetRTTTextures( std::vector< osg::ref_ptr< osg::Texture2D > >& depthTexture )
{
    m_rttTextures = depthTexture;
    
}
////////////////////////////////////////////////////////////////////////////////
std::vector< osg::ref_ptr< osg::Texture2D > > SceneManager::GetRTTTextures()
{
    return m_rttTextures;
}

void SceneManager::ShowSelectionLine( gadget::Keys key, int x, int y, int state )
{
    ;
}

void SceneManager::HideSelectionLine( gadget::Keys key, int x, int y, int state )
{
    ;
}

void SceneManager::SetSelectionLineStartEndPoint( osg::Vec3d start, osg::Vec3d end )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
