// --- My Includes --- //
#include "Grid.h"

// --- OSG Includes ---//
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/LineWidth>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace Construction;

////////////////////////////////////////////////////////////////////////////////
Grid::Grid()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Grid::Grid( int gridSize, std::map< std::pair< int, int >, bool > occMatrix )
{
    CreateGrid( gridSize, occMatrix );
}
////////////////////////////////////////////////////////////////////////////////
Grid::Grid( const Grid& grid, const osg::CopyOp& copyop )
:
osg::Geode( grid, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Grid::~Grid()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void Grid::CreateGrid( int gridSize, std::map< std::pair< int, int >, bool > occMatrix )
{
    float halfGridSize = gridSize * 0.5f;

    osg::ref_ptr< osg::Geometry > grid = new osg::Geometry();
    osg::ref_ptr< osg::Geometry > platform = new osg::Geometry();
    osg::ref_ptr< osg::Geometry > walls = new osg::Geometry();
    osg::ref_ptr< osg::Geometry > lines = new osg::Geometry();

    osg::ref_ptr< osg::Vec3Array > gridVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > platformVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > lineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > wallVertices = new osg::Vec3Array();

    for( int j = 0; j < gridSize; ++j )
    {
	    for( int i = 0; i < gridSize; ++i )
	    {
		    gridVertices->push_back( osg::Vec3( i -       halfGridSize,
                                               -j +       halfGridSize, 0.0f ) );
		    gridVertices->push_back( osg::Vec3( i -       halfGridSize,
                                               -j - 1.0 + halfGridSize, 0.0f ) );
		    gridVertices->push_back( osg::Vec3( i + 1.0 - halfGridSize,
                                               -j - 1.0 + halfGridSize, 0.0f ) );
		    gridVertices->push_back( osg::Vec3( i + 1.0 - halfGridSize,
                                               -j +       halfGridSize, 0.0f ) );
	    }
    }

    platformVertices->push_back( osg::Vec3( -halfGridSize,  halfGridSize,  0.0f ) );
    platformVertices->push_back( osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize,  0.0f ) );

    platformVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize,  0.0f ) );
    platformVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3(  halfGridSize, -halfGridSize,  0.0f ) );

    platformVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize,  0.0f ) );
    platformVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3( halfGridSize,  halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3( halfGridSize,  halfGridSize,  0.0f ) );

    platformVertices->push_back( osg::Vec3(  halfGridSize, halfGridSize,  0.0f ) );
    platformVertices->push_back( osg::Vec3(  halfGridSize, halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize,  0.0f ) );

    platformVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3(  halfGridSize,  halfGridSize, -gridSize * 0.05f ) );
    platformVertices->push_back( osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05f ) );

    for( int i = 0; i <= gridSize; ++i )
    {
	    lineVertices->push_back( osg::Vec3( -halfGridSize, -i + halfGridSize, 0.001f ) );
	    lineVertices->push_back( osg::Vec3(  halfGridSize, -i + halfGridSize, 0.001f ) );

	    lineVertices->push_back( osg::Vec3( i - halfGridSize,  halfGridSize, 0.001f ) );
	    lineVertices->push_back( osg::Vec3( i - halfGridSize, -halfGridSize, 0.001f ) );
   }
	
    lineVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize,  0.0f ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize,  0.0f ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize,  0.0f ) );
    lineVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3( halfGridSize, halfGridSize,  0.0f ) );
    lineVertices->push_back( osg::Vec3( halfGridSize, halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05f ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05f ) );
    lineVertices->push_back( osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, -gridSize * 0.05f ) );
    lineVertices->push_back( osg::Vec3( halfGridSize,  halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3(  halfGridSize, halfGridSize, -gridSize * 0.05f ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, -gridSize * 0.05f ) );

    lineVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, 0.0f ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, halfGridSize ) );

    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, 0.0f ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, halfGridSize ) );

    lineVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, 0.0f ) );
    lineVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, halfGridSize ) );

    lineVertices->push_back( osg::Vec3( halfGridSize, halfGridSize, 0.0f ) );
    lineVertices->push_back( osg::Vec3( halfGridSize, halfGridSize, halfGridSize ) );

    lineVertices->push_back( osg::Vec3( -halfGridSize,  halfGridSize, halfGridSize ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, halfGridSize ) );

    lineVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, halfGridSize ) );
    lineVertices->push_back( osg::Vec3(  halfGridSize, -halfGridSize, halfGridSize ) );

    lineVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, halfGridSize ) );
    lineVertices->push_back( osg::Vec3( halfGridSize,  halfGridSize, halfGridSize ) );

    lineVertices->push_back( osg::Vec3(  halfGridSize, halfGridSize, halfGridSize ) );
    lineVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, halfGridSize ) );

    wallVertices->push_back( osg::Vec3( -halfGridSize,  halfGridSize, halfGridSize ) );
    wallVertices->push_back( osg::Vec3( -halfGridSize,  halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, halfGridSize ) );

    wallVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, halfGridSize ) );
    wallVertices->push_back( osg::Vec3( -halfGridSize, -halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3(  halfGridSize, -halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3(  halfGridSize, -halfGridSize, halfGridSize ) );
			
    wallVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, halfGridSize ) );
    wallVertices->push_back( osg::Vec3( halfGridSize, -halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3( halfGridSize,  halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3( halfGridSize,  halfGridSize, halfGridSize ) );

    wallVertices->push_back( osg::Vec3(  halfGridSize, halfGridSize, halfGridSize ) );
    wallVertices->push_back( osg::Vec3(  halfGridSize, halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, 0.0f ) );
    wallVertices->push_back( osg::Vec3( -halfGridSize, halfGridSize, halfGridSize ) );

    grid->setVertexArray( gridVertices.get() );
    platform->setVertexArray( platformVertices.get() );
    lines->setVertexArray( lineVertices.get() );
    walls->setVertexArray( wallVertices.get() );

    osg::ref_ptr< osg::Vec4Array > gridColor = new osg::Vec4Array();
    std::map< std::pair< int, int >, bool >::const_iterator itr;
    for( itr = occMatrix.begin(); itr != occMatrix.end(); ++itr )
    {
	    if( itr->second == true )
	    {
		    gridColor->push_back( osg::Vec4( 0.7f, 0.7f, 0.7f, 1.0f ) );
	    }

	    else if( itr->second == false )
	    {
		    gridColor->push_back( osg::Vec4( 0.4f, 0.4f, 0.4f, 1.0f ) );
	    }
    }

    grid->setColorArray( gridColor.get() );
    grid->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    osg::ref_ptr< osg::Vec4Array > platformColor = new osg::Vec4Array();
    platformColor->push_back( osg::Vec4( 0.2f, 0.2f, 0.2f, 1.0f ) );
    platform->setColorArray( platformColor.get() );
    platform->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec4Array > lineColor = new osg::Vec4Array();
    lineColor->push_back( osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    lines->setColorArray( lineColor.get() );
    lines->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec4Array > wallColor = new osg::Vec4Array();
    wallColor->push_back( osg::Vec4( 0.3f, 0.3f, 0.3f, 0.3f ) );
    walls->setColorArray( wallColor.get() );
    walls->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr<osg::Vec3Array> gridNormals = new osg::Vec3Array();
    gridNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    grid->setNormalArray( gridNormals.get() );
    grid->setNormalBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array >  platformNormals = new osg::Vec3Array();
    platformNormals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 1.0f, 0.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 0.0f, 1.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 0.0f, 0.0f, -1.0f ) );
    platform->setNormalArray( platformNormals.get() );
    platform->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    lines->setNormalArray( lineNormals.get() );
    lines->setNormalBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > wallNormals = new osg::Vec3Array();
    wallNormals->push_back( osg::Vec3( 1.0f, 0.0f, 0.0f ) );
    wallNormals->push_back( osg::Vec3( 0.0f, 1.0f, 0.0f ) );
    wallNormals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );
    wallNormals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );
    walls->setNormalArray( wallNormals.get() );
    walls->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    osg::ref_ptr< osg::StateSet > lineStateSet = new osg::StateSet();
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 4.0f );
    lineStateSet->setAttribute( lineWidth.get() );
    lines->setStateSet( lineStateSet.get() );

    osg::ref_ptr< osg::StateSet > wallStateSet = new osg::StateSet();
    wallStateSet->setMode( GL_BLEND, osg::StateAttribute::ON );
    wallStateSet->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    walls->setStateSet( wallStateSet.get() );

    grid->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, gridVertices->size() ) );
    platform->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, platformVertices->size() ) );
    lines->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, lineVertices->size() ) );
    walls->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, wallVertices->size() ) );

    addDrawable( grid.get() );
    addDrawable( platform.get() );
    addDrawable( lines.get() );
    addDrawable( walls.get() );
}
////////////////////////////////////////////////////////////////////////////////
