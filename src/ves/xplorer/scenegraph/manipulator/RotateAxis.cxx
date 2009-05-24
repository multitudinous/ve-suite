/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/RotateAxis.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/PolygonStipple>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateAxis::RotateAxis()
    :
    Dragger()
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
RotateAxis::RotateAxis(
    const RotateAxis& rotateAxis, const osg::CopyOp& copyop )
    :
    Dragger( rotateAxis, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateAxis::~RotateAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void RotateAxis::SetupDefaultGeometry()
{
    size_t numSegments = 100;
    double innerRadius = 0.95;
    double outerRadius = 1.00;
    double deltaIncrement =
        ( 2.0 * osg::PI ) / static_cast< double >( numSegments );

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    size_t numVertices = numSegments + 2;
    vertices->resize( numVertices );
    vertices->push_back( osg::Vec3( 0.0, 0.0, 0.0 ) );
    for( size_t i = 0; i <= numSegments; ++i )
    {
        double rot = static_cast< double >( i ) * deltaIncrement;
        double cosVal = cos( rot );
        double sinVal = sin( rot );

        double y = outerRadius * cosVal;
        double z = outerRadius * sinVal;

        vertices->push_back( osg::Vec3( 0.0, y, z ) );
    }

    geometry->setVertexArray( vertices.get() );
    geometry->addPrimitiveSet(
        new osg::DrawArrays(
            osg::PrimitiveSet::TRIANGLE_FAN, 0, vertices->size() ) );
    geometry->addPrimitiveSet(
        new osg::DrawArrays(
            osg::PrimitiveSet::LINES, 1, vertices->size() ) );
    geode->addDrawable( geometry.get() );

    //Triangle Strip
    /*
    size_t numVertices = 2 * ( numSegments + 1 );
    vertices->resize( numVertices );
    for( size_t i = 0; i < numVertices; i += 2 )
    {
        double rot = static_cast< double >( i ) * deltaIncrement;
        double cosVal = cos( rot );
        double sinVal = sin( rot );

        double yi = innerRadius * cosVal;
        double zi = innerRadius * sinVal;

        double yo = outerRadius * cosVal;
        double zo = outerRadius * sinVal;

        (*vertices)[ i ] = osg::Vec3( 0.0, yi, zi );
        (*vertices)[ i + 1 ] = osg::Vec3( 0.0, yo, zo );
    }

    geometry->setVertexArray( vertices.get() );
    geometry->addPrimitiveSet(
        new osg::DrawArrays(
            osg::PrimitiveSet::TRIANGLE_STRIP, 0, vertices->size() ) );
    geode->addDrawable( geometry.get() );
    */

    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();

    //Set line width
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0 );
    stateSet->setAttributeAndModes( lineWidth.get(), osg::StateAttribute::ON );

    //stateSet->setMode( GL_LINE_SMOOTH,
        //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    //osg::ref_ptr< osg::Hint > hint =
        //new osg::Hint( GL_LINE_SMOOTH_HINT, GL_NICEST );
    //stateSet->setAttributeAndModes( hint.get(),
        //osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    //Set polygon stipple
    osg::ref_ptr< osg::PolygonStipple > polygonStipple =
        new osg::PolygonStipple();
    GLubyte halftone[] =
    {
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55
    };
    polygonStipple->setMask( halftone );
    stateSet->setAttributeAndModes( polygonStipple.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    stateSet->setMode( GL_POLYGON_SMOOTH,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    osg::ref_ptr< osg::Hint > hint =
        new osg::Hint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
    stateSet->setAttributeAndModes( hint.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    //Add rotation axis to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
