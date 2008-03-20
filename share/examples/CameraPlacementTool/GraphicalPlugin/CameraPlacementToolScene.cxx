// --- My Includes --- //
#include "CameraPlacementToolScene.h"
#include "CameraPlacementToolShaders.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Texture2D>
#include <osg/TexGenNode>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolScene::CameraPlacementToolScene(
    ves::xplorer::scenegraph::DCS* pluginDCS )
:
m_textureUnit( 0 ),
mPluginDCS( pluginDCS ),
m_torus( new ves::xplorer::scenegraph::DCS() ),
mCameraEntity( new cpt::CameraEntity( pluginDCS ) )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolScene::~CameraPlacementToolScene()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolScene::CreateShaderPrograms()
{
    osg::ref_ptr< osg::Shader > hightlightVertex = new osg::Shader();
    hightlightVertex->setType( osg::Shader::VERTEX );
    hightlightVertex->setShaderSource( highlight_vertex );

    osg::ref_ptr< osg::Shader > highlightFragment = new osg::Shader();
    highlightFragment->setType( osg::Shader::FRAGMENT );
    highlightFragment->setShaderSource( highlight_fragment );

    osg::ref_ptr< osg::Program > highlightProgram = new osg::Program();
    highlightProgram->addShader( hightlightVertex.get() );
    highlightProgram->addShader( highlightFragment.get() );
    boost::any anyVal = highlightProgram;
    ves::xplorer::scenegraph::ResourceManager::instance()->add(
        std::string( "HighlightProgram" ), anyVal );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolScene::CreateProjectionTexture()
{
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );

    stateset->setAttribute(
        ( ves::xplorer::scenegraph::ResourceManager::instance()->get
        < osg::Program, osg::ref_ptr >( "HighlightProgram" ) ).get(),
        osg::StateAttribute::ON );

    m_torus->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
cpt::CameraEntity* CameraPlacementToolScene::GetActiveCameraEntity()
{
    return mCameraEntity.get();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolScene::Initialize()
{
    //Store the shader programs for the scene
    CreateShaderPrograms();

    mCameraEntity->SetNameAndDescriptions( std::string( "Camera" ) );

    m_torus->setPosition( osg::Vec3d( 0, 10, 0 ) );
    osg::ref_ptr< osg::Node > node =
        osgDB::readNodeFile( std::string( "Models/torus.ive" ) );
    m_torus->addChild( node.get() );
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    m_torus->setDescriptions( descriptorsList );
    m_torus->setName( std::string( "Torus" ) );
    mPluginDCS->AddChild( m_torus.get() );

    CreateProjectionTexture();
}
////////////////////////////////////////////////////////////////////////////////
