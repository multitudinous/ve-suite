// --- My Includes --- //
#include "Scene.h"
#include "CameraEntity.h"
#include "Shaders.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Texture2D>
#include <osg/TexGenNode>
#include <osg/CullFace>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>

#include <gmtl/Xforms.h>

// --- Bullet Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
Scene::Scene( ves::xplorer::scenegraph::DCS* pluginDCS )
:
m_pluginDCS( pluginDCS ),
m_torus( new ves::xplorer::scenegraph::DCS() ),
m_camera( new cpt::CameraEntity( pluginDCS ) )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Scene::~Scene()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Scene::Initialize()
{
    m_camera->SetNameAndDescriptions( std::string( "Camera" ) );

    m_torus->setPosition( osg::Vec3d( 0, 10, 0 ) );
    osg::ref_ptr< osg::Node > node = osgDB::readNodeFile( std::string( "Models/torus.ive" ) );
    m_torus->addChild( node.get() );
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    m_torus->setDescriptions( descriptorsList );
    m_torus->setName( std::string( "Torus" ) );
    m_pluginDCS->AddChild( m_torus.get() );

    CreateProjectionTexture();
}
////////////////////////////////////////////////////////////////////////////////
void Scene::CreateProjectionTexture()
{
    //Create a 2D texture
    osg::ref_ptr< osg::Texture2D > texture2D = new osg::Texture2D();
    texture2D->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    texture2D->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    texture2D->setWrap( osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_BORDER );
    texture2D->setWrap( osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_BORDER );

    osg::ref_ptr< osg::Image > image2D = osgDB::readImageFile( "Textures/highlight.tga" );
    texture2D->setImage( image2D.get() );

    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setTextureAttributeAndModes( 0, texture2D.get(), osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_S, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_T, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_R, osg::StateAttribute::ON );
    stateset->setTextureMode( 0, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    osg::ref_ptr< osg::Shader > vertexShader = new osg::Shader();
    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    vertexShader->setType( osg::Shader::VERTEX );
    fragmentShader->setType( osg::Shader::FRAGMENT );
    vertexShader->setShaderSource( vertex_shader );
    fragmentShader->setShaderSource( fragment_shader );
    program->addShader( vertexShader.get() );
    program->addShader( fragmentShader.get() );
    stateset->setAttribute( program.get() );

    osg::ref_ptr< osg::Uniform > projectionMap = new osg::Uniform( "projectionMap", 0 );
    stateset->addUniform( projectionMap.get() );

    m_torus->setStateSet( stateset.get() );
}
////////////////////////////////////////////////////////////////////////////////
