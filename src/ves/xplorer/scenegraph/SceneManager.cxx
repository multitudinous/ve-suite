/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
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

#include <ves/xplorer/scenegraph/logo/BlueArrow.h>
#include <ves/xplorer/scenegraph/logo/GreyArrow.h>
#include <ves/xplorer/scenegraph/logo/OrangeArrow.h>
#include <ves/xplorer/scenegraph/logo/VE.h>
#include <ves/xplorer/scenegraph/logo/Suite.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Group>
#include <osg/Node>
#include <osgDB/Registry>
#include <osgDB/ReaderWriter>
#include <osgDB/WriteFile>
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/Switch>
#endif

#ifdef VE_SOUND
// --- osgAL Includes --- //
#include <osgAL/SoundManager>
#include <osgAL/SoundRoot>
#include <osgAL/SoundNode>
#include <osgAL/SoundState>
#endif

// --- C/C++ Libraries --- //
#include <iostream>
#include <string>
#include <istream>
#include <sstream>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
vprSingletonImpLifetime( SceneManager, 100 );

SceneManager::SceneManager()
        :
#ifdef VE_SOUND
        m_sound( 0 ),
#endif
        m_blueArrow( 0 ),
        m_greyArrow( 0 ),
        m_orangeArrow( 0 ),
        m_veText( 0 ),
        m_suiteText( 0 )
{
    _param.erase();
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::Initialize( std::string param )
{
    _param = param;
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

    rootNode = new ves::xplorer::scenegraph::Group();
    rootNode->SetName( "Root Node" );
    rootNode->setThreadSafeRefUnref( true );

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
        rootNode->addChild( soundRoot.get() );
    }
    catch( ... )
    {
        std::cerr << "|\tosgAL::SoundManager is unable to initialize." 
            << std::endl;
    }
#endif

    worldDCS = new ves::xplorer::scenegraph::DCS();
    worldDCS->SetName( "World DCS" );

    networkDCS  = new ves::xplorer::scenegraph::DCS();
    networkDCS->SetName( "Network DCS" );

    m_clrNode = new osg::ClearNode();
    m_clrNode->setRequiresClear( true );
    m_clrNode->setClearColor( osg::Vec4( 0.0f, 0.0f, 0.0f, 1.0f ) );
    m_clrNode->setName( "Clear Node - Control ClearColor" );

    //Create the switch for our logo
    _createLogo();

#ifdef VE_SOUND
    //m_sound = new ves::xplorer::scenegraph::Sound( _logoNode.get() );
    //m_sound->LoadFile( "C:/TSVEG/Dependencies/osgal-0.6.1/data/bee.wav" );
#endif

    _logoSwitch->AddChild( worldDCS.get() );
    _logoSwitch->AddChild( _logoNode.get() );
    _logoSwitch->AddChild( networkDCS.get() );

    ///World DCS
    m_matrixStore[ 0 ] = gmtl::Matrix44d();
    ///Logo DCS
    m_matrixStore[ 1 ] = gmtl::Matrix44d();
    ///Network DCS
    m_matrixStore[ 2 ] = gmtl::Matrix44d();

    //m_oqc = new osgOQ::OcclusionQueryContext();
    ///number of pixels
    //m_oqc->setVisibilityThreshold( 1000 );
    ///Number of verts
    //m_oqc->setOccluderThreshold( 1000 );
    ///Specifies how many frames to wait before issuing another query
    //m_oqc->setQueryFrameCount( 3 );
    ///Specify whether to use hierarchical ("NonFlat") placement for
    //m_oqc->setNonFlatPlacement( true );
    ///Place bounding volumes in for osgOQ nodes
    //m_oqc->setDebugDisplay( true );
    // Sets the debug verbosity. Currently supported 'level' values:
    //    0 -- Verbosity is controlled by osg::notify.
    //    1 -- For each OQN in each frame, displays whether that node
    //         thinks its actual geometry is visible or not and why.
    // Call through OcclusionQueryRoot to set value only for a
    //   specific number of frames.
    //void setDebugVerbosity( 0 );
    //m_oqc->setStatistics( true );

    rootNode->addChild( m_clrNode.get() );
    m_clrNode->addChild( _logoSwitch.get() );

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
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Group* SceneManager::GetRootNode()
{
    return rootNode.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* SceneManager::GetWorldDCS()
{
    return worldDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* SceneManager::GetNetworkDCS()
{
    return networkDCS.get();
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
    if( !_logoSwitch )
    {
        _logoSwitch = new ves::xplorer::scenegraph::Switch();
    }

    if( !_logoNode.valid() )
    {
        double translation[ 3 ] = { -1.7, 4.6, 3.1 };
        osg::Quat quat( -1.0, osg::Vec3( 0, 0, 1 ) );
        double scale[ 3 ] = { 0.0065, 0.0065, 0.0065 };

        _logoNode = new ves::xplorer::scenegraph::DCS();
        _logoNode->SetTranslationArray( translation );
        _logoNode->SetQuat( quat );
        _logoNode->SetScaleArray( scale );

        //m_blueArrow = new ves::xplorer::scenegraph::CADEntity( 
        //    BlueArrow(), _logoNode.get(), true, false );
        //m_greyArrow = new ves::xplorer::scenegraph::CADEntity( 
        //    GreyArrow(), _logoNode.get(), true, false );
        //m_orangeArrow = new ves::xplorer::scenegraph::CADEntity( 
        //    OrangeArrow(), _logoNode.get(), true, false );
        m_veText = new ves::xplorer::scenegraph::CADEntity( 
            VE(), _logoNode.get(), true, false );
        m_suiteText = new ves::xplorer::scenegraph::CADEntity( 
            Suite(), _logoNode.get(), true, false );

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

            "vec3 color=TotalAmbient+TotalDiffuse+TotalSpecular; \n"

            "gl_FragColor=vec4(color,1.0); \n"
            "} \n";

        osg::ref_ptr< osg::StateSet > stateset = _logoNode->getOrCreateStateSet();
        osg::ref_ptr< osg::Program > program = new osg::Program;

        osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, phong_vertex );
        program->addShader( vertex_shader.get() );

        osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, phong_fragment );
        program->addShader( fragment_shader.get() );

        stateset->setAttribute( program.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetActiveSwitchNode( int activeNode )
{
    //GetActiveSwitchNode()->GetMat();
    _logoSwitch->SetVal( activeNode );
    ///Now reset the dcs back to its former position so that the nav
    ///information is defined on a per node basis.
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* SceneManager::GetActiveSwitchNode()
{
    osg::Switch::ValueList boolList = _logoSwitch->getValueList();

    for( size_t i = 0; i < boolList.size(); ++i )
    {
        if( boolList.at( i ) )
        {
            return dynamic_cast< ves::xplorer::scenegraph::DCS* >( _logoSwitch->getChild( i ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::SetBackgroundColor( std::vector< double > color )
{
    m_clrNode->setClearColor( osg::Vec4( color.at( 0 ), color.at( 1 ), color.at( 2 ), 1.0f ) );
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::Shutdown()
{
#ifdef VE_SOUND
    osgAL::SoundManager::instance()->shutdown();
#endif
}
////////////////////////////////////////////////////////////////////////////////
/*osgOQ::OcclusionQueryContext* SceneManager::GetOcclusionQueryContext()
{
    return m_oqc.get();
}
////////////////////////////////////////////////////////////////////////////////
void SceneManager::ResetOcclusionQueryContext()
{
    m_oqc = new osgOQ::OcclusionQueryContext();
    ///number of pixels
    m_oqc->setVisibilityThreshold( 500 );
    ///Number of verts
    m_oqc->setOccluderThreshold( 1000 );
    ///Specifies how many frames to wait before issuing another query
    m_oqc->setQueryFrameCount( 5 );
    ///Specify whether to use hierarchical ("NonFlat") placement for
    m_oqc->setNonFlatPlacement( true );
    ///Place bounding volumes in for osgOQ nodes
    //m_oqc->setDebugDisplay( true );
    // Sets the debug verbosity. Currently supported 'level' values:
    //    0 -- Verbosity is controlled by osg::notify.
    //    1 -- For each OQN in each frame, displays whether that node
    //         thinks its actual geometry is visible or not and why.
    // Call through OcclusionQueryRoot to set value only for a
    //   specific number of frames.
    //void setDebugVerbosity( 0 );
    m_oqc->setStatistics( true );
}*/

} // end scenegraph
} // end xplorer
} // end ves
