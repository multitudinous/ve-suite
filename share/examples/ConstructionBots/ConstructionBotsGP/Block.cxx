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
    //Create the block
    mDrawables[ 4 ] = new osg::Geometry();
    osg::ref_ptr< osg::StateSet > blockStateSet = new osg::StateSet();
    blockStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    mDrawables[ 4 ]->setStateSet( blockStateSet.get() );

    osg::ref_ptr< osg::Vec3Array > blockVertices = new osg::Vec3Array();
    //Left
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    //Near
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f,  0.5f ) );
    //Right
    blockVertices->push_back( osg::Vec3( 0.5f, -0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( 0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( 0.5f,  0.5f,  0.5f ) );
    //Far
    blockVertices->push_back( osg::Vec3(  0.5f, 0.5f,  0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, 0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, 0.5f,  0.5f ) );
    //Top
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f,  0.5f, 0.5f ) );
    //Bottom
    blockVertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3( -0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f,  0.5f, -0.5f ) );
    blockVertices->push_back( osg::Vec3(  0.5f, -0.5f, -0.5f ) );
    mDrawables[ 4 ]->setVertexArray( blockVertices.get() );

    osg::ref_ptr< osg::Vec4Array > blockColor = new osg::Vec4Array();
    blockColor->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    mDrawables[ 4 ]->setColorArray( blockColor.get() );
    mDrawables[ 4 ]->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::Vec3Array > blockNormals = new osg::Vec3Array();
    //Left
    blockNormals->push_back( osg::Vec3( -1.0f,  0.0f,  0.0f ) );
    //Near
    blockNormals->push_back( osg::Vec3(  0.0f, -1.0f,  0.0f ) );
    //Right
    blockNormals->push_back( osg::Vec3(  1.0f,  0.0f,  0.0f ) );
    //Far
    blockNormals->push_back( osg::Vec3(  0.0f,  1.0f,  0.0f ) );
    //Top
    blockNormals->push_back( osg::Vec3(  0.0f,  0.0f,  1.0f ) );
    //Bottom
    blockNormals->push_back( osg::Vec3(  0.0f,  0.0f, -1.0f ) );
    mDrawables[ 4 ]->setNormalArray( blockNormals.get() );
    mDrawables[ 4 ]->setNormalBinding(
        osg::Geometry::BIND_PER_PRIMITIVE );

    mDrawables[ 4 ]->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, blockVertices.get()->size() ) );
    addDrawable( mDrawables[ 4 ].get() );

    //Create the lines
    mDrawables[ 2 ] = new osg::Geometry();
    mDrawables[ 3 ] = new osg::Geometry();
    mDrawables[ 0 ] = new osg::Geometry();
    mDrawables[ 1 ] = new osg::Geometry();

    osg::ref_ptr< osg::StateSet > lineStateSet = new osg::StateSet();
    lineStateSet->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    lineStateSet->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0f );
    lineStateSet->setAttribute( lineWidth.get() );

    osg::ref_ptr< osg::Vec3Array > leftLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > nearLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > rightLineVertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec3Array > farLineVertices = new osg::Vec3Array();
    //Left
    leftLineVertices->push_back( osg::Vec3( -0.5f,  0.5f, 0.5f ) );
    leftLineVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    mDrawables[ 2 ]->setVertexArray( leftLineVertices.get() );
    //Near
    nearLineVertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
    nearLineVertices->push_back( osg::Vec3(  0.5f, -0.5f, 0.5f ) );
    mDrawables[ 3 ]->setVertexArray( nearLineVertices.get() );
    //Right
    rightLineVertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
    rightLineVertices->push_back( osg::Vec3( 0.5f,  0.5f, 0.5f ) );
    mDrawables[ 0 ]->setVertexArray( rightLineVertices.get() );
    //Far
    farLineVertices->push_back( osg::Vec3(  0.5f, 0.5f, 0.5f ) );
    farLineVertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );
    mDrawables[ 1 ]->setVertexArray( farLineVertices.get() );

    mDrawables[ 2 ]->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::LINES, 0, leftLineVertices->size() ) );
    mDrawables[ 3 ]->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::LINES, 0, nearLineVertices->size() ) );
    mDrawables[ 0 ]->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::LINES, 0, rightLineVertices->size() ) );
    mDrawables[ 1 ]->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::LINES, 0, farLineVertices->size() ) );

    std::map< unsigned int, osg::ref_ptr< osg::Geometry > >::const_iterator itr;
    for( itr = mDrawables.begin(); itr != mDrawables.find( 4 ); ++itr )
    {
        osg::ref_ptr< osg::Vec4Array > lineColor = new osg::Vec4Array();
        lineColor->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
        itr->second->setColorArray( lineColor.get() );
        itr->second->setColorBinding( osg::Geometry::BIND_OVERALL );

        itr->second->setStateSet( lineStateSet.get() );
        addDrawable( itr->second.get() );
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
