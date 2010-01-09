
// --- ParallaxMapping Includes --- //
#include "ParallaxMappingGP.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/EnvironmentHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/scenegraph/util/TSGVisitor.h>

#include <ves/xplorer/scenegraph/technique/ParallaxMapping.h>

// --- OSG Includes --- //

#include <osgDB/ReadFile>

////////////////////////////////////////////////////////////////////////////////
ParallaxMappingGP::ParallaxMappingGP()
    :
    PluginBase(),
    m_cadEntity( NULL )
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "ParallaxMapping";
}
////////////////////////////////////////////////////////////////////////////////
ParallaxMappingGP::~ParallaxMappingGP()
{
    if( mSceneManager )
    {
        osg::ref_ptr< osg::Group > rootNode =
            mSceneManager->GetRootNode();

        if( rootNode.valid() )
        {
            if( m_cadEntity->GetDCS() )
            {
                rootNode->removeChild( m_cadEntity->GetDCS() );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMappingGP::InitializeNode( osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    /*
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->push_back( osg::Vec3( -1.0, -1.0, 0.0 ) );
    vertices->push_back( osg::Vec3(  1.0, -1.0, 0.0 ) );
    vertices->push_back( osg::Vec3(  1.0,  1.0, 0.0 ) );
    vertices->push_back( osg::Vec3( -1.0,  1.0, 0.0 ) );
    geometry->setVertexArray( vertices.get() );

    osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array();
    normals->push_back( osg::Vec3( 0.0, 0.0, 1.0 ) );
    normals->push_back( osg::Vec3( 0.0, 0.0, 1.0 ) );
    normals->push_back( osg::Vec3( 0.0, 0.0, 1.0 ) );
    normals->push_back( osg::Vec3( 0.0, 0.0, 1.0 ) );
    geometry->setNormalArray( normals.get() );
    geometry->setNormalBinding( osg::Geometry::BIND_PER_VERTEX );

    osg::ref_ptr< osg::Vec2Array > texCoord = new osg::Vec2Array();
    texCoord->push_back( osg::Vec2d( 0.0, 0.0 ) );
    texCoord->push_back( osg::Vec2d( 1.0, 0.0 ) );
    texCoord->push_back( osg::Vec2d( 1.0, 1.0 ) );
    texCoord->push_back( osg::Vec2d( 0.0, 1.0 ) );
    geometry->setTexCoordArray( 1, texCoord.get() );

    geometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, vertices.get()->size() ) );
    geode->addDrawable( geometry.get() );
    */

    m_cadEntity = new ves::xplorer::scenegraph::CADEntity(
        "Models/IVEs/sphere.ive", mDCS.get() );

    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    m_cadEntity->GetDCS()->setDescriptions( descriptorsList );
    m_cadEntity->GetDCS()->setName( "sphere" );

    ves::xplorer::scenegraph::technique::ParallaxMapping* parallaxMapping =
        new ves::xplorer::scenegraph::technique::ParallaxMapping(
            m_cadEntity->GetDCS() );
    parallaxMapping->SetBaseMap(
        osgDB::readImageFile( "Textures/rock_diffuse.png" ) );
    parallaxMapping->SetNormalMap(
        osgDB::readImageFile( "Textures/rock_bump.png" ) );
    parallaxMapping->SetHeightMap(
        osgDB::readImageFile( "Textures/rock_height.png" ) );

    m_cadEntity->GetDCS()->AddTechnique( "Select", parallaxMapping );
    m_cadEntity->GetDCS()->AddTechnique( "ParallaxMapping", parallaxMapping );
    m_cadEntity->GetDCS()->SetTechnique( "ParallaxMapping" );
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMappingGP::PreFrameUpdate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ParallaxMappingGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////