/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

// --- My Includes --- //
#include "Grid.h"

// --- OSG Includes ---//
#include <osg/Geometry>
#include <osg/PolygonOffset>
#include <osg/LineWidth>

#include <osgDB/ReadFile>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
Grid::Grid()
{
    Initialize();
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
void Grid::Initialize()
{

}
////////////////////////////////////////////////////////////////////////////////
void Grid::CreateGrid(
    int gridSize, std::map< std::pair< int, int >,
                            std::pair< bool, bool > >& occupancyMatrix )
{
    float halfGridSize = gridSize * 0.5;

    osg::ref_ptr< osg::Geometry > grid = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > gridStateSet = new osg::StateSet();
    gridStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    osg::ref_ptr< osg::PolygonOffset > polyoffset = new osg::PolygonOffset();
    polyoffset->setFactor( 1.0 );
    polyoffset->setUnits( 1.0 );
    gridStateSet->setAttributeAndModes( polyoffset.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    grid->setStateSet( gridStateSet.get() );

    osg::ref_ptr< osg::Vec3Array > gridVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > gridColor = new osg::Vec4Array();
    for( int j = 0; j < gridSize; ++j )
    {
        for( int i = 0; i < gridSize; ++i )
        {
            double x =  i - halfGridSize;
            double y = -j + halfGridSize;
            gridVertices->push_back( osg::Vec3( x,       y,       0.0 ) );
            gridVertices->push_back( osg::Vec3( x,       y - 1.0, 0.0 ) );
            gridVertices->push_back( osg::Vec3( x + 1.0, y - 1.0, 0.0 ) );
            gridVertices->push_back( osg::Vec3( x + 1.0, y,       0.0 ) );

            std::pair< int, int > location = std::make_pair(
                static_cast< int >( x + 0.5 ), static_cast< int >( y - 0.5 ) );
            std::map< std::pair< int, int >, std::pair< bool, bool > >::
                const_iterator itr = occupancyMatrix.find( location );
            if( itr != occupancyMatrix.end() )
            {
                bool occupancy = itr->second.first;

                if( occupancy )
                {
                    gridColor->push_back( osg::Vec4( 0.7, 0.7, 0.7, 1.0 ) );
                }
                else
                {
                    gridColor->push_back( osg::Vec4( 0.4, 0.4, 0.4, 1.0 ) );
                }
            }
        }
    }
    grid->setVertexArray( gridVertices.get() );
    grid->setColorArray( gridColor.get() );
    grid->setColorBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    osg::ref_ptr< osg::Vec3Array > gridNormals = new osg::Vec3Array();
    gridNormals->push_back( osg::Vec3( 0.0, 0.0, 1.0 ) );
    grid->setNormalArray( gridNormals.get() );
    grid->setNormalBinding( osg::Geometry::BIND_OVERALL );

    grid->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, gridVertices->size() ) );
    addDrawable( grid.get() );

    osg::ref_ptr< osg::Geometry > platform = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > platformStateSet = new osg::StateSet();
    platformStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    platformStateSet->setAttributeAndModes( polyoffset.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    osg::ref_ptr< osg::Vec3Array > platformVertices = new osg::Vec3Array();
    platformVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize,  0.0 ) );
    platformVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize,  0.0 ) );

    platformVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize,  0.0 ) );
    platformVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize,  0.0 ) );

    platformVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize,  0.0 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize,  0.0 ) );

    platformVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize,  0.0 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize,  0.0 ) );

    platformVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    platformVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    platform->setVertexArray( platformVertices.get() );

    osg::ref_ptr< osg::Vec4Array > platformColor = new osg::Vec4Array();
    platformColor->push_back( osg::Vec4( 0.2, 0.2, 0.2, 1.0 ) );
    platform->setColorArray( platformColor.get() );
    platform->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array >  platformNormals = new osg::Vec3Array();
    platformNormals->push_back( osg::Vec3( -1.0,  0.0,  0.0 ) );
    platformNormals->push_back( osg::Vec3(  0.0, -1.0,  0.0 ) );
    platformNormals->push_back( osg::Vec3(  1.0,  0.0,  0.0 ) );
    platformNormals->push_back( osg::Vec3(  0.0,  1.0,  0.0 ) );
    platformNormals->push_back( osg::Vec3(  0.0,  0.0, -1.0 ) );
    platform->setNormalArray( platformNormals.get() );
    platform->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    platform->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, platformVertices->size() ) );
    addDrawable( platform.get() );

    osg::ref_ptr< osg::Geometry > lines = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > lineStateSet = new osg::StateSet();
    lineStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0 );
    lineStateSet->setAttribute( lineWidth.get() );
    lines->setStateSet( lineStateSet.get() );

    osg::ref_ptr< osg::Vec3Array > lineVertices = new osg::Vec3Array();
    for( int i = 0; i <= gridSize; ++i )
    {
        lineVertices->push_back(
            osg::Vec3( -halfGridSize, -i + halfGridSize, 0.0 ) );
        lineVertices->push_back(
            osg::Vec3(  halfGridSize, -i + halfGridSize, 0.0 ) );

        lineVertices->push_back(
            osg::Vec3( i - halfGridSize,  halfGridSize, 0.0 ) );
        lineVertices->push_back(
            osg::Vec3( i - halfGridSize, -halfGridSize, 0.0 ) );
    }

    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, -gridSize * 0.05 ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, -gridSize * 0.05 ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, -gridSize * 0.05 ) );

    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize,  halfGridSize ) );

    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize,  halfGridSize ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize,  halfGridSize ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize,  0.0 ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize,  halfGridSize ) );

    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize,  halfGridSize ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize,  halfGridSize ) );

    lineVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize,  halfGridSize ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize,  halfGridSize ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize,  halfGridSize ) );
    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize,  halfGridSize ) );

    lineVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize,  halfGridSize ) );
    lineVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize,  halfGridSize ) );
    lines->setVertexArray( lineVertices.get() );

    osg::ref_ptr< osg::Vec4Array > lineColor = new osg::Vec4Array();
    lineColor->push_back( osg::Vec4( 0.5, 0.5, 0.5, 1.0 ) );
    lines->setColorArray( lineColor.get() );
    lines->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    lineNormals->push_back( osg::Vec3( 0.0, 0.0, 1.0 ) );
    lines->setNormalArray( lineNormals.get() );
    lines->setNormalBinding( osg::Geometry::BIND_OVERALL );

    lines->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::LINES, 0, lineVertices->size() ) );
    addDrawable( lines.get() );

    osg::ref_ptr< osg::Geometry > walls = new osg::Geometry();
    walls->setName( std::string( "Wall" ) );
    osg::ref_ptr< osg::StateSet > wallStateSet = new osg::StateSet();
    wallStateSet->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    wallStateSet->setMode( GL_BLEND, osg::StateAttribute::ON );
    walls->setStateSet( wallStateSet.get() );

    osg::ref_ptr< osg::Vec3Array > wallVertices = new osg::Vec3Array();
    wallVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, halfGridSize ) );
    wallVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, halfGridSize ) );

    wallVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, halfGridSize ) );
    wallVertices->push_back(
        osg::Vec3( -halfGridSize, -halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, halfGridSize ) );

    wallVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, halfGridSize ) );
    wallVertices->push_back(
        osg::Vec3(  halfGridSize, -halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, halfGridSize ) );

    wallVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, halfGridSize ) );
    wallVertices->push_back(
        osg::Vec3(  halfGridSize,  halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, 0.0 ) );
    wallVertices->push_back(
        osg::Vec3( -halfGridSize,  halfGridSize, halfGridSize ) );
    walls->setVertexArray( wallVertices.get() );

    osg::ref_ptr< osg::Vec4Array > wallColor = new osg::Vec4Array();
    wallColor->push_back( osg::Vec4( 0.3, 0.3, 0.3, 0.3 ) );
    walls->setColorArray( wallColor.get() );
    walls->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > wallNormals = new osg::Vec3Array();
    wallNormals->push_back( osg::Vec3(  1.0,  0.0, 0.0 ) );
    wallNormals->push_back( osg::Vec3(  0.0,  1.0, 0.0 ) );
    wallNormals->push_back( osg::Vec3( -1.0,  0.0, 0.0 ) );
    wallNormals->push_back( osg::Vec3(  0.0, -1.0, 0.0 ) );
    walls->setNormalArray( wallNormals.get() );
    walls->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    walls->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, wallVertices->size() ) );
    addDrawable( walls.get() );
}
////////////////////////////////////////////////////////////////////////////////
