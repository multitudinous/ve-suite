/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef WIN32
#include <sys/types.h>
//biv--check here if build/run problems occur
#endif

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#ifdef VE_SOUND
#include <ves/xplorer/scenegraph/Sound.h>
#endif

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/logo/BlueArrow.h>
#include <ves/xplorer/scenegraph/logo/GreyArrow.h>
#include <ves/xplorer/scenegraph/logo/OrangeArrow.h>
#include <ves/xplorer/scenegraph/logo/VE.h>
#include <ves/xplorer/scenegraph/logo/Suite.h>

#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/Group>
#include <osg/Switch>
#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>

#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/WriteFile>

#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAudio/SoundManager.h>
#include <osgAudio/SoundRoot.h>
#include <osgAudio/SoundNode.h>
#include <osgAudio/SoundState.h>
#endif //VE_SOUND

// --- VR Juggler Includes --- //
#include <jccl/RTRC/ConfigManager.h>

#include <gmtl/gmtl.h>
#include <gmtl/Misc/MatrixConvert.h>

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
    m_blueArrow( NULL ),
    m_greyArrow( NULL ),
    m_orangeArrow( NULL ),
    m_veText( NULL ),
    m_suiteText( NULL ),
    m_clrNode( NULL ),
    m_clearColorUniform(
        new osg::Uniform( "clearColor", osg::Vec4( 0.0, 0.0, 0.0, 0.0 ) ) ),
    mFrameStamp( NULL ),
    mCharacterController( NULL ),
    m_isRTTOn( false ),
    m_isDesktopMode( false ),
    m_screenAlignedNormals( true ),
    m_isMasterNode( true )
{
    ;
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
    //Do nothing right now
    if( m_blueArrow )
    {
        delete m_blueArrow;
        m_blueArrow = 0;
    }

    if( m_greyArrow )
    {
        delete m_greyArrow;
        m_greyArrow = 0;
    }

    if( m_orangeArrow )
    {
        delete m_orangeArrow;
        m_orangeArrow = 0;
    }

    if( m_veText )
    {
        delete m_veText;
        m_veText = 0;
    }

    if( m_suiteText )
    {
        delete m_suiteText;
        m_suiteText = 0;
    }

#ifdef VE_SOUND
    //delete m_sound;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::InitScene()
{
    std::cout << 
        "|  1. Initializing.................................... SceneManager |" 
        << std::endl;

    m_vrjHead.init( "VJHead" );

    //mRootNode = new ves::xplorer::scenegraph::Group();
    if( !mRootNode.valid() )
    {
        std::cout << " big problems " << std::endl;
        exit( 1 );
    }
    mRootNode->setName( "Root Node" );
    mRootNode->setThreadSafeRefUnref( true );

    mModelRoot = new osg::Group();
    //mModelRoot = new ves::xplorer::scenegraph::DCS();
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
    mNavSwitch = new ves::xplorer::scenegraph::Switch();
    mNavSwitch->setName( "Nav Switch" );
    
    m_navDCS = new ves::xplorer::scenegraph::DCS();
    m_navDCS->SetName( "World DCS" );
    //Setup world nav switch
    mNavSwitch->addChild( m_navDCS.get() );
    
    //Setup logo nav switch
    mNavSwitch->addChild( new ves::xplorer::scenegraph::DCS() );

    mNetworkDCS  = new osg::Group();
    mNetworkDCS->setName( "Network DCS" );
    //Setup network nav switch
    mNavSwitch->addChild( new ves::xplorer::scenegraph::DCS() );

    m_clrNode = new osg::ClearNode();
    m_clrNode->setRequiresClear( true );
    m_clrNode->setClearColor( osg::Vec4( 0.0, 0.0, 0.0, 0.0 ) );
    m_clrNode->setName( "Clear Node - Control ClearColor" );

    //Create the switch for our logo
    _createLogo();

#ifdef VE_SOUND
    //m_sound = new ves::xplorer::scenegraph::Sound( mLogoNode.get() );
    //m_sound->LoadFile( "C:/TSVEG/Dependencies/osgal-0.6.1/data/bee.wav" );
#endif

    mLogoSwitch->addChild( mModelRoot.get() );
    mLogoSwitch->addChild( mLogoNode.get() );
    mLogoSwitch->addChild( mNetworkDCS.get() );

    mRootNode->addChild( m_clrNode.get() );
    m_clrNode->addChild( mLogoSwitch.get() );
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
osg::Group* const SceneManager::GetRootNode() const
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
osg::Group* const SceneManager::GetModelRoot() const
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
DCS* const SceneManager::GetNavDCS() const
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
osg::Group* const SceneManager::GetNetworkDCS() const
{
    return mNetworkDCS.get();
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
        mLogoSwitch = new ves::xplorer::scenegraph::Switch();
    }

    if( mLogoNode.valid() )
    {
        return;
    }

    mLogoNode = new ves::xplorer::scenegraph::DCS();
    if( IsDesktopMode() )
    {
        double translation[ 3 ] = { -1.7, 4.6, 0.0 };
        mLogoNode->SetTranslationArray( translation );
    }
    else
    {
        double translation[ 3 ] = { -1.7, 4.6, 3.1 };
        mLogoNode->SetTranslationArray( translation );
    }

    osg::Quat quat( -1.0, osg::Vec3( 0, 0, 1 ) );
    double scale[ 3 ] = { 0.0065, 0.0065, 0.0065 };
    mLogoNode->SetQuat( quat );
    mLogoNode->SetScaleArray( scale );
    mLogoNode->setName( "Logo Node" );

    //m_blueArrow = new ves::xplorer::scenegraph::CADEntity( 
    //    BlueArrow(), mLogoNode.get(), true, "Off" );
    //m_greyArrow = new ves::xplorer::scenegraph::CADEntity( 
    //    GreyArrow(), mLogoNode.get(), true, "Off" );
    //m_orangeArrow = new ves::xplorer::scenegraph::CADEntity( 
    //    OrangeArrow(), mLogoNode.get(), true, "Off" );
    m_veText = new ves::xplorer::scenegraph::CADEntity( 
        VE(), mLogoNode.get(), true, "Off" );
    m_suiteText = new ves::xplorer::scenegraph::CADEntity( 
        Suite(), mLogoNode.get(), true, "Off" );

    char phong_vertex[] =
        "varying vec4 color; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
        "gl_Position=ftransform(); \n"

        "color=gl_Color; \n"
        "eyePos=vec3(gl_ModelViewMatrix*gl_Vertex); \n"
        "lightPos=gl_LightSource[0].position.xyz; \n"
        "normal=vec3(gl_NormalMatrix*gl_Normal); \n"
        "} \n";

    char phong_fragment[] =
        "uniform vec3 glowColor; \n"

        "varying vec4 color; \n"
        "varying vec3 eyePos; \n"
        "varying vec3 lightPos; \n"
        "varying vec3 normal; \n"

        "void main() \n"
        "{ \n"
        "vec3 N=normalize(normal); \n"
        "vec3 L=normalize(lightPos); \n"
        "float NDotL=max(dot(N,L),0.0); \n"

        "vec3 V=normalize(eyePos); \n"
        "vec3 R=reflect(V,N); \n"
        "float RDotL=max(dot(R,L),0.0); \n"

        "vec3 TotalAmbient=gl_LightSource[0].ambient.rgb*color.rgb; \n"
        "vec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*color.rgb*NDotL; \n"
        "vec3 TotalSpecular=gl_LightSource[0].specular.rgb*color.rgb*pow(RDotL,20.0); \n"

        "vec3 temp=TotalAmbient+TotalDiffuse+TotalSpecular; \n"

        "gl_FragData[ 0 ] = vec4( temp, 1.0 ); \n"
        "gl_FragData[ 1 ] = vec4( glowColor, gl_FragData[ 0 ].a ); \n"
        "} \n";

    osg::ref_ptr< osg::StateSet > stateset = mLogoNode->getOrCreateStateSet();
    osg::ref_ptr< osg::Program > program = new osg::Program;

    osg::ref_ptr< osg::Shader > vertex_shader = 
        new osg::Shader( osg::Shader::VERTEX, phong_vertex );
    program->addShader( vertex_shader.get() );

    osg::ref_ptr< osg::Shader > fragment_shader = 
        new osg::Shader( osg::Shader::FRAGMENT, phong_fragment );
    program->addShader( fragment_shader.get() );

    stateset->setAttribute( program.get() );
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
void SceneManager::LatePreFrameUpdate()
{
    if( !mNavSwitch->getValue( 1 ) )
    {
        gmtl::Matrix44d navMatrix = mActiveNavDCS->GetMat();
        gmtl::invert( m_invertedNavMatrix, navMatrix );
        m_invertedNavMatrixOSG.set( m_invertedNavMatrix.mData );
        
        m_vrjHeadMatrix = 
            gmtl::convertTo< double >( m_vrjHead->getData() );
        const gmtl::AxisAngled myAxisAngle( 
            osg::DegreesToRadians( double( 90 ) ), 1, 0, 0 );
        const gmtl::Matrix44d myMat = 
            gmtl::make< gmtl::Matrix44d >( myAxisAngle );
        m_vrjHeadMatrix = myMat * m_vrjHeadMatrix;
        m_globalViewMatrix =  m_invertedNavMatrix * m_vrjHeadMatrix;
        m_globalViewMatrixOSG.set( m_globalViewMatrix.mData );
        
        gmtl::invert( m_invertedGlobalViewMatrix, m_globalViewMatrix );
        m_invertedGlobalViewMatrixOSG.set( m_globalViewMatrix.mData );     
    }
    else
    {
        m_invertedNavMatrix = gmtl::identity( m_invertedNavMatrix );
        static_cast< ves::xplorer::scenegraph::DCS* >( 
            mNavSwitch->getChild( 1 ) )->SetMat( m_invertedNavMatrix );
    }
    
    m_cameraManager->LatePreFrameUpdate();
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
osg::Group* const SceneManager::GetActiveSwitchNode() const
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
DCS* const SceneManager::GetActiveNavSwitchNode() const
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
    osg::Vec4 clearColor( color.at( 0 ), color.at( 1 ), color.at( 2 ), 0.0 );
    m_clrNode->setClearColor( clearColor );
    m_clearColorUniform->set( clearColor );
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
osg::FrameStamp* const SceneManager::GetFrameStamp() const
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
bool SceneManager::IsDesktopMode()
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
