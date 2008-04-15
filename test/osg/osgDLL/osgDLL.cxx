// --- My Includes --- //
#include "osgDLL.h"

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgDB/ReadFile>

////////////////////////////////////////////////////////////////////////////////
void* CreateOSGGeode()
{
    osg::Geode* geode = new osg::Geode();
    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    geometry->setStateSet( stateset.get() );

    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    //Left
    vertices->push_back( osg::Vec3( -0.5f,  0.5f,  0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    //Near
    vertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3(  0.5f, -0.5f,  0.5f ) );	
    //Right
    vertices->push_back( osg::Vec3( 0.5f, -0.5f,  0.5f ) );
    vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3( 0.5f,  0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3( 0.5f,  0.5f,  0.5f ) );
    //Far
    vertices->push_back( osg::Vec3(  0.5f, 0.5f,  0.5f ) );
    vertices->push_back( osg::Vec3(  0.5f, 0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f, 0.5f,  0.5f ) );	
    //Top
    vertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    vertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );
    vertices->push_back( osg::Vec3(  0.5f,  0.5f, 0.5f ) );
    //Bottom
    vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3(  0.5f,  0.5f, -0.5f ) );
    vertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );
    geometry->setVertexArray( vertices.get() );

	osg::ref_ptr< osg::Vec4Array > color = new osg::Vec4Array();
	color->push_back( osg::Vec4( 0.5, 1.0, 0.5, 1.0 ) );
    geometry->setColorArray( color.get() );
    geometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array();
    //Left
    normals->push_back( osg::Vec3( -1.0f,  0.0f,  0.0f ) );
    //Near
    normals->push_back( osg::Vec3(  0.0f, -1.0f,  0.0f ) );
    //Right
    normals->push_back( osg::Vec3(  1.0f,  0.0f,  0.0f ) );
    //Far
    normals->push_back( osg::Vec3(  0.0f,  1.0f,  0.0f ) );
    //Top
    normals->push_back( osg::Vec3(  0.0f,  0.0f,  1.0f ) );
    //Bottom
    normals->push_back( osg::Vec3(  0.0f,  0.0f, -1.0f ) );
    geometry->setNormalArray( normals.get() );
    geometry->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    geometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, vertices->size() ) );
    geode->addDrawable( geometry.get() );

    return geode;
}
////////////////////////////////////////////////////////////////////////////////
