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
    int gridSize, std::map< std::pair< int, int >, bool > occMatrix )
{
    //osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
    //stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    //setStateSet( stateset.get() );

    float halfGridSize = gridSize * 0.5f;

    osg::ref_ptr< osg::Geometry > grid = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > gridStateSet = new osg::StateSet();
    gridStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    osg::ref_ptr< osg::PolygonOffset > polyoffset = new osg::PolygonOffset();
    polyoffset->setFactor( 1.0f );
    polyoffset->setUnits( 1.0f );
    gridStateSet->setAttributeAndModes( polyoffset.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    grid->setStateSet( gridStateSet.get() );

    osg::ref_ptr< osg::Vec3Array > gridVertices = new osg::Vec3Array();
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
    grid->setVertexArray( gridVertices.get() );

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

    osg::ref_ptr< osg::Vec3Array > gridNormals = new osg::Vec3Array();
    gridNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
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
    platform->setVertexArray( platformVertices.get() );

    osg::ref_ptr< osg::Vec4Array > platformColor = new osg::Vec4Array();
    platformColor->push_back( osg::Vec4( 0.2f, 0.2f, 0.2f, 1.0f ) );
    platform->setColorArray( platformColor.get() );
    platform->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array >  platformNormals = new osg::Vec3Array();
    platformNormals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 1.0f, 0.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 0.0f, 1.0f, 0.0f ) );
    platformNormals->push_back( osg::Vec3( 0.0f, 0.0f, -1.0f ) );
    platform->setNormalArray( platformNormals.get() );
    platform->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    platform->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, platformVertices->size() ) );
    addDrawable( platform.get() );

    osg::ref_ptr< osg::Geometry > lines = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > lineStateSet = new osg::StateSet();
    lineStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0f );
    lineStateSet->setAttribute( lineWidth.get() );
    lines->setStateSet( lineStateSet.get() );

    osg::ref_ptr< osg::Vec3Array > lineVertices = new osg::Vec3Array();
    for( int i = 0; i <= gridSize; ++i )
    {
        lineVertices->push_back( osg::Vec3( -halfGridSize, -i + halfGridSize, 0.0f ) );
        lineVertices->push_back( osg::Vec3(  halfGridSize, -i + halfGridSize, 0.0f ) );

        lineVertices->push_back( osg::Vec3( i - halfGridSize,  halfGridSize, 0.0f ) );
        lineVertices->push_back( osg::Vec3( i - halfGridSize, -halfGridSize, 0.0f ) );
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
    lines->setVertexArray( lineVertices.get() );

    osg::ref_ptr< osg::Vec4Array > lineColor = new osg::Vec4Array();
    lineColor->push_back( osg::Vec4( 0.5f, 0.5f, 0.5f, 1.0f ) );
    lines->setColorArray( lineColor.get() );
    lines->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > lineNormals = new osg::Vec3Array();
    lineNormals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );
    lines->setNormalArray( lineNormals.get() );
    lines->setNormalBinding( osg::Geometry::BIND_OVERALL );

    lines->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::LINES, 0, lineVertices->size() ) );
    addDrawable( lines.get() );

    osg::ref_ptr< osg::Geometry > walls = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > wallStateSet = new osg::StateSet();
    wallStateSet->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    wallStateSet->setMode( GL_BLEND, osg::StateAttribute::ON );
    walls->setStateSet( wallStateSet.get() );

    osg::ref_ptr< osg::Vec3Array > wallVertices = new osg::Vec3Array();
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
    walls->setVertexArray( wallVertices.get() );

    osg::ref_ptr< osg::Vec4Array > wallColor = new osg::Vec4Array();
    wallColor->push_back( osg::Vec4( 0.3f, 0.3f, 0.3f, 0.3f ) );
    walls->setColorArray( wallColor.get() );
    walls->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > wallNormals = new osg::Vec3Array();
    wallNormals->push_back( osg::Vec3( 1.0f, 0.0f, 0.0f ) );
    wallNormals->push_back( osg::Vec3( 0.0f, 1.0f, 0.0f ) );
    wallNormals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );
    wallNormals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );
    walls->setNormalArray( wallNormals.get() );
    walls->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

    walls->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, wallVertices->size() ) );
    addDrawable( walls.get() );
}
////////////////////////////////////////////////////////////////////////////////
