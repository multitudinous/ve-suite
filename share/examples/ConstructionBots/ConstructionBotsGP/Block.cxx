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
#include "Block.h"

// --- OSG Includes --- //
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/PolygonOffset>

using namespace bots;

////////////////////////////////////////////////////////////////////////////////
Block::Block()
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Block::Block( const Block& block, const osg::CopyOp& copyop )
    :
    osg::Geode( block, copyop )
{
    if( &block != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
Block::~Block()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Block::Initialize()
{
    //Create the lines
    mDrawables[ 0 ] = new osg::Geometry();
    mDrawables[ 1 ] = new osg::Geometry();
    mDrawables[ 2 ] = new osg::Geometry();
    mDrawables[ 3 ] = new osg::Geometry();

    osg::ref_ptr< osg::StateSet > lineStateSet = new osg::StateSet();
    lineStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    lineStateSet->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0 );
    lineStateSet->setAttribute( lineWidth.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Vec3Array > rightLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > farLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > leftLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > nearLineVertices = new osg::Vec3Array();

    //Right
    rightLineVertices->push_back( osg::Vec3d( 0.5, -0.5, 0.5 ) );
    rightLineVertices->push_back( osg::Vec3d( 0.5,  0.5, 0.5 ) );
    mDrawables[ 0 ]->setVertexArray( rightLineVertices.get() );
    //Far
    farLineVertices->push_back( osg::Vec3d(  0.5, 0.5, 0.5 ) );
    farLineVertices->push_back( osg::Vec3d( -0.5, 0.5, 0.5 ) );
    mDrawables[ 1 ]->setVertexArray( farLineVertices.get() );
    //Left
    leftLineVertices->push_back( osg::Vec3d( -0.5,  0.5, 0.5 ) );
    leftLineVertices->push_back( osg::Vec3d( -0.5, -0.5, 0.5 ) );
    mDrawables[ 2 ]->setVertexArray( leftLineVertices.get() );
    //Near
    nearLineVertices->push_back( osg::Vec3d( -0.5, -0.5, 0.5 ) );
    nearLineVertices->push_back( osg::Vec3d(  0.5, -0.5, 0.5 ) );
    mDrawables[ 3 ]->setVertexArray( nearLineVertices.get() );

    std::map< unsigned int, osg::ref_ptr< osg::Geometry > >::const_iterator lgi;
    for( lgi = mDrawables.begin(); lgi != mDrawables.find( 4 ); ++lgi )
    {
        lgi->second->setStateSet( lineStateSet.get() );

        osg::ref_ptr< osg::Vec4Array > lineColor = new osg::Vec4Array();
        lineColor->push_back( osg::Vec4d( 0.5, 0.5, 0.5, 1.0 ) );
        lgi->second->setColorArray( lineColor.get() );
        lgi->second->setColorBinding( osg::Geometry::BIND_OVERALL );

        lgi->second->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        addDrawable( lgi->second.get() );
    }

    //Create the block
    mDrawables[ 4 ] = new osg::Geometry();
    mDrawables[ 4 ]->setName( std::string( "BlockRight" ) );
    mDrawables[ 5 ] = new osg::Geometry();
    mDrawables[ 5 ]->setName( std::string( "BlockFar" ) );
    mDrawables[ 6 ] = new osg::Geometry();
    mDrawables[ 6 ]->setName( std::string( "BlockLeft" ) );
    mDrawables[ 7 ] = new osg::Geometry();
    mDrawables[ 7 ]->setName( std::string( "BlockNear" ) );
    mDrawables[ 8 ] = new osg::Geometry();
    mDrawables[ 8 ]->setName( std::string( "BlockTop" ) );
    mDrawables[ 9 ] = new osg::Geometry();
    mDrawables[ 9 ]->setName( std::string( "BlockBottom" ) );

    osg::ref_ptr< osg::StateSet > blockStateSet = new osg::StateSet();
    blockStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    osg::ref_ptr< osg::PolygonOffset > polyoffset = new osg::PolygonOffset();
    polyoffset->setFactor( 1.0f );
    polyoffset->setUnits( 1.0f );
    blockStateSet->setAttributeAndModes( polyoffset.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    osg::ref_ptr< osg::Vec3Array > rightBlockVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > farBlockVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > leftBlockVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > nearBlockVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > topBlockVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > bottomBlockVertices = new osg::Vec3Array();
    //Right
    rightBlockVertices->push_back( osg::Vec3d( 0.5, -0.5,  0.5 ) );
    rightBlockVertices->push_back( osg::Vec3d( 0.5, -0.5, -0.5 ) );
    rightBlockVertices->push_back( osg::Vec3d( 0.5,  0.5, -0.5 ) );
    rightBlockVertices->push_back( osg::Vec3d( 0.5,  0.5,  0.5 ) );
    mDrawables[ 4 ]->setVertexArray( rightBlockVertices.get() );
    //Far
    farBlockVertices->push_back( osg::Vec3d(  0.5, 0.5,  0.5 ) );
    farBlockVertices->push_back( osg::Vec3d(  0.5, 0.5, -0.5 ) );
    farBlockVertices->push_back( osg::Vec3d( -0.5, 0.5, -0.5 ) );
    farBlockVertices->push_back( osg::Vec3d( -0.5, 0.5,  0.5 ) );
    mDrawables[ 5 ]->setVertexArray( farBlockVertices.get() );
    //Left
    leftBlockVertices->push_back( osg::Vec3d( -0.5,  0.5,  0.5 ) );
    leftBlockVertices->push_back( osg::Vec3d( -0.5,  0.5, -0.5 ) );
    leftBlockVertices->push_back( osg::Vec3d( -0.5, -0.5, -0.5 ) );
    leftBlockVertices->push_back( osg::Vec3d( -0.5, -0.5,  0.5 ) );
    mDrawables[ 6 ]->setVertexArray( leftBlockVertices.get() );
    //Near
    nearBlockVertices->push_back( osg::Vec3d( -0.5, -0.5,  0.5 ) );
    nearBlockVertices->push_back( osg::Vec3d( -0.5, -0.5, -0.5 ) );
    nearBlockVertices->push_back( osg::Vec3d(  0.5, -0.5, -0.5 ) );
    nearBlockVertices->push_back( osg::Vec3d(  0.5, -0.5,  0.5 ) );
    mDrawables[ 7 ]->setVertexArray( nearBlockVertices.get() );
    //Top
    topBlockVertices->push_back( osg::Vec3d( -0.5,  0.5, 0.5 ) );
    topBlockVertices->push_back( osg::Vec3d( -0.5, -0.5, 0.5 ) );
    topBlockVertices->push_back( osg::Vec3d(  0.5, -0.5, 0.5 ) );
    topBlockVertices->push_back( osg::Vec3d(  0.5,  0.5, 0.5 ) );
    mDrawables[ 8 ]->setVertexArray( topBlockVertices.get() );
    //Bottom
    bottomBlockVertices->push_back( osg::Vec3d( -0.5, -0.5, -0.5 ) );
    bottomBlockVertices->push_back( osg::Vec3d( -0.5,  0.5, -0.5 ) );
    bottomBlockVertices->push_back( osg::Vec3d(  0.5,  0.5, -0.5 ) );
    bottomBlockVertices->push_back( osg::Vec3d(  0.5, -0.5, -0.5 ) );
    mDrawables[ 9 ]->setVertexArray( bottomBlockVertices.get() );

    osg::ref_ptr< osg::Vec3Array > rightBlockNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > farBlockNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > leftBlockNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > nearBlockNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > topBlockNormals = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > bottomBlockNormals = new osg::Vec3Array();
    //Right
    rightBlockNormals->push_back( osg::Vec3d( 1.0, 0.0, 0.0 ) );
    //Far
    farBlockNormals->push_back( osg::Vec3d( 0.0, 1.0, 0.0 ) );
    //Left
    leftBlockNormals->push_back( osg::Vec3d( -1.0, 0.0, 0.0 ) );
    //Near
    nearBlockNormals->push_back( osg::Vec3d( 0.0, -1.0, 0.0 ) );
    //Top
    topBlockNormals->push_back( osg::Vec3d( 0.0, 0.0, 1.0 ) );
    //Bottom
    bottomBlockNormals->push_back( osg::Vec3d( 0.0, 0.0, -1.0 ) );

    mDrawables[ 4 ]->setNormalArray( rightBlockNormals.get() );
    mDrawables[ 5 ]->setNormalArray( farBlockNormals.get() );
    mDrawables[ 6 ]->setNormalArray( leftBlockNormals.get() );
    mDrawables[ 7 ]->setNormalArray( nearBlockNormals.get() );
    mDrawables[ 8 ]->setNormalArray( topBlockNormals.get() );
    mDrawables[ 9 ]->setNormalArray( bottomBlockNormals.get() );

    std::map< unsigned int, osg::ref_ptr< osg::Geometry > >::const_iterator bgi;
    for( bgi = mDrawables.find( 4 ); bgi != mDrawables.end(); ++bgi )
    {
        bgi->second->setStateSet( blockStateSet.get() );

        osg::ref_ptr< osg::Vec4Array > color = new osg::Vec4Array();
        color->push_back( osg::Vec4d( 1.0, 1.0, 1.0, 1.0 ) );
        bgi->second->setColorArray( color.get() );
        bgi->second->setColorBinding( osg::Geometry::BIND_OVERALL );

        bgi->second->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

        bgi->second->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, 4 ) );

        addDrawable( bgi->second.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Block::SetColor( unsigned int drawable, const osg::Vec4& color )
{
    osg::ref_ptr< osg::Vec4Array > colorArray =
        static_cast< osg::Vec4Array* >(
            mDrawables[ drawable ]->asGeometry()->getColorArray() );

    if( colorArray.valid() )
    {
        colorArray->at( 0 ) = color;
        mDrawables[ drawable ]->dirtyDisplayList();
    }
}
////////////////////////////////////////////////////////////////////////////////
