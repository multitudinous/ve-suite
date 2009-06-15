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

#ifndef WIN32
#include <sys/types.h>
//biv--check here if build/run problems occur
#else
//#include <windows.h>
#endif

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>
#ifdef VE_SOUND
#include <ves/xplorer/scenegraph/Sound.h>
#endif

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/logo/BlueArrow.h>
#include <ves/xplorer/scenegraph/logo/GreyArrow.h>
#include <ves/xplorer/scenegraph/logo/OrangeArrow.h>
#include <ves/xplorer/scenegraph/logo/VE.h>
#include <ves/xplorer/scenegraph/logo/Suite.h>

#include <ves/xplorer/scenegraph/physics/CharacterController.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

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
#include <osgAL/SoundManager>
#include <osgAL/SoundRoot>
#include <osgAL/SoundNode>
#include <osgAL/SoundState>
#endif //VE_SOUND

// --- VR Juggler Includes --- //
#include <jccl/RTRC/ConfigManager.h>

#include <gmtl/gmtl.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <string>
#include <istream>
#include <sstream>

using namespace ves::xplorer::scenegraph;
namespace vxs = ves::xplorer::scenegraph;

vprSingletonImpLifetime( SceneManager, 20 );

////////////////////////////////////////////////////////////////////////////////
SceneManager::SceneManager()
    :
    mRootNode( NULL ),
    mModelRoot( NULL ),
    m_manipulatorManager( NULL ),
    mLogoNode( NULL ),
    mLogoSwitch( NULL ),
    mNavSwitch( NULL ),
    mActiveNavDCS( NULL ),
    worldDCS( NULL ),
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
    mFrameStamp( NULL ),
    mCharacterController( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::Initialize( std::string& param )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SceneManager::~SceneManager()
{
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
    delete m_sound;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::InitScene()
{
    std::cout << 
        "|  1. Initializing.................................... SceneManager |" 
        << std::endl;

    //mRootNode = new ves::xplorer::scenegraph::Group();
    mRootNode->setName( "Root Node" );
    mRootNode->setThreadSafeRefUnref( true );

    mModelRoot = new osg::Group();
    //mModelRoot = new ves::xplorer::scenegraph::DCS();
    mModelRoot->setName( "Model Root Node" );

    m_manipulatorManager = new manipulator::ManipulatorManager();
    m_manipulatorManager->setName( "Manipulator Manager Node" );
    mModelRoot->addChild( m_manipulatorManager.get() );

#ifdef VE_SOUND
    try
    {
        osgAL::SoundManager::instance()->init( 32 );
        osgAL::SoundManager::instance()->getEnvironment()->
            setDistanceModel( openalpp::InverseDistance );
        osgAL::SoundManager::instance()->getEnvironment()->
            setDopplerFactor( 1 );
        
        osg::ref_ptr< osgAL::SoundRoot > soundRoot = new osgAL::SoundRoot();
        soundRoot->setName( "Sound Root" );
        mRootNode->addChild( soundRoot.get() );
    }
    catch( ... )
    {
        std::cerr << "|\tosgAL::SoundManager is unable to initialize." 
            << std::endl;
    }
#endif
    mNavSwitch = new ves::xplorer::scenegraph::Switch();
    mNavSwitch->setName( "Nav Switch" );
    
    worldDCS = new ves::xplorer::scenegraph::DCS();
    worldDCS->SetName( "World DCS" );
    //Setup world nav switch
    mNavSwitch->addChild( worldDCS.get() );
    
    //Setup logo nav switch
    mNavSwitch->addChild( new ves::xplorer::scenegraph::DCS() );

    mNetworkDCS  = new osg::Group();
    mNetworkDCS->setName( "Network DCS" );
    //Setup network nav switch
    mNavSwitch->addChild( new ves::xplorer::scenegraph::DCS() );

    m_clrNode = new osg::ClearNode();
    m_clrNode->setRequiresClear( true );
    m_clrNode->setClearColor( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
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
    //Add the worlddcs here because the nav matrix is pulled out
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
    mCharacterController = new vxs::CharacterController();
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
osg::Group* const SceneManager::GetModelRoot() const
{
    return mModelRoot.get();
}
////////////////////////////////////////////////////////////////////////////////
manipulator::ManipulatorManager* const SceneManager::GetManipulatorManager() const
{
    return m_manipulatorManager.get();
}
////////////////////////////////////////////////////////////////////////////////
DCS* const SceneManager::GetWorldDCS() const
{
    return worldDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
const gmtl::Matrix44d& SceneManager::GetInvertedWorldDCS() const
{
    return mInvertedWorldDCS;
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

    if( !mLogoNode.valid() )
    {
        double translation[ 3 ] = { -1.7, 4.6, 3.1 };
        osg::Quat quat( -1.0, osg::Vec3( 0, 0, 1 ) );
        double scale[ 3 ] = { 0.0065, 0.0065, 0.0065 };

        mLogoNode = new ves::xplorer::scenegraph::DCS();
        mLogoNode->SetTranslationArray( translation );
        mLogoNode->SetQuat( quat );
        mLogoNode->SetScaleArray( scale );

        //m_blueArrow = new ves::xplorer::scenegraph::CADEntity( 
        //    BlueArrow(), mLogoNode.get(), true, false );
        //m_greyArrow = new ves::xplorer::scenegraph::CADEntity( 
        //    GreyArrow(), mLogoNode.get(), true, false );
        //m_orangeArrow = new ves::xplorer::scenegraph::CADEntity( 
        //    OrangeArrow(), mLogoNode.get(), true, false );
        m_veText = new ves::xplorer::scenegraph::CADEntity( 
            VE(), mLogoNode.get(), true, false );
        m_suiteText = new ves::xplorer::scenegraph::CADEntity( 
            Suite(), mLogoNode.get(), true, false );

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
            "uniform vec4 glowColor; \n"

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
            "gl_FragData[ 1 ] = glowColor; \n"
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
void SceneManager::PreFrameUpdate()
{
    if( !mNavSwitch->getValue( 1 ) )
    {
        mInvertedWorldDCS = mActiveNavDCS->GetMat();
        mInvertedWorldDCS = gmtl::invert( mInvertedWorldDCS );
    }
    else
    {
        mInvertedWorldDCS = gmtl::identity( mInvertedWorldDCS );
        static_cast< ves::xplorer::scenegraph::DCS* >( 
            mNavSwitch->getChild( 1 ) )->SetMat( mInvertedWorldDCS );
    }
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
    m_clrNode->setClearColor(
        osg::Vec4( color.at( 0 ), color.at( 1 ), color.at( 2 ), 1.0f ) );
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::Shutdown()
{
#ifdef VE_SOUND
    osgAL::SoundManager::instance()->shutdown();
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
CharacterController* const SceneManager::GetCharacterController() const
{
    return mCharacterController;
}
////////////////////////////////////////////////////////////////////////////////
