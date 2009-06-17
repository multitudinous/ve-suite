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
 * Date modified: $Date: 2009-05-26 17:48:25 -0600 (Tue, 26 May 2009) $
 * Version:       $Rev: 12728 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: RotateTwist.cxx 12728 2009-05-26 23:48:25Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>
#include <ves/xplorer/scenegraph/manipulator/ClippingCircle.h>
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geometry>
#include <osg/Billboard>
#include <osg/LineWidth>
#include <osg/PolygonStipple>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateTwist::RotateTwist( Manipulator* parentManipulator )
    :
    Dragger( parentManipulator )
{
    m_transformationType = TransformationType::ROTATE_TWIST;

    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist::RotateTwist(
    const RotateTwist& rotateTwist, const osg::CopyOp& copyop )
    :
    Dragger( rotateTwist, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist::~RotateTwist()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) )
    {
        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
}
////////////////////////////////////////////////////////////////////////////////
const char* RotateTwist::className() const
{
    return "RotateTwist";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateTwist::clone( const osg::CopyOp& copyop ) const
{
    return new RotateTwist( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateTwist::cloneType() const
{
    return new RotateTwist( m_parentManipulator );
}
////////////////////////////////////////////////////////////////////////////////
bool RotateTwist::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const RotateTwist* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* RotateTwist::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::SetupDefaultGeometry()
{
    size_t numSegments( 100 );
    double radius( 1.2 );
    double TWO_PI( 2.0 * osg::PI );
    double ringDelta( TWO_PI / numSegments );

    //The geode to add the geometry to
    osg::ref_ptr< osg::Billboard > billboard = new osg::Billboard();
    billboard->setMode( osg::Billboard::POINT_ROT_EYE );

    //Create the rotation twist axis with line loops
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        for( size_t i = 0; i < numSegments; ++i )
        {
            double rot( i * ringDelta );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double x( radius * cosVal );
            double z( radius * sinVal );

            vertices->push_back( osg::Vec3( x, 0.0, z ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::LINE_LOOP, 0, vertices->size() ) );

        billboard->addDrawable( geometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            geometry->getOrCreateStateSet();

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( 2.0 );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );

        //Set line hints
        stateSet->setMode( GL_LINE_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_LINE_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create invisible triangle strip for picking the rotation twist axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        double minorRadius( 0.025 );
        double innerRadius( radius - minorRadius );
        double outerRadius( radius + minorRadius );
        for( size_t i = 0; i <= numSegments; ++i )
        {
            double rot( i * ringDelta );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double xi( innerRadius * cosVal );
            double zi( innerRadius * sinVal );

            double xo( outerRadius * cosVal );
            double zo( outerRadius * sinVal );

            vertices->push_back( osg::Vec3( xi, 0.0, zi ) );
            vertices->push_back( osg::Vec3( xo, 0.0, zo ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_STRIP, 0, vertices->size() ) );

        SetDrawableToAlwaysCull( *geometry.get() );
        billboard->addDrawable( geometry.get() );
    }

    //Create stippled geometry to show rotation about the twist axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        vertices->push_back( osg::Vec3( 0.0, 0.0, 0.0 ) );
        for( size_t i = 0; i <= numSegments; ++i )
        {
            double rot( i * ringDelta );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double x( cosVal );
            double z( sinVal );

            vertices->push_back( osg::Vec3( x, 0.0, z ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_FAN, 0, vertices->size() ) );

        //billboard->addDrawable( geometry.get() );
        
        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            geometry->getOrCreateStateSet();

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

        //Set polygon hints
        stateSet->setMode( GL_POLYGON_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create a clipping circle
    {
        osg::ref_ptr< ClippingCircle > clippingCircle = new ClippingCircle();
        addChild( clippingCircle.get() );
    }

    //Add rotation axis to the scene
    addChild( billboard.get() );
}
////////////////////////////////////////////////////////////////////////////////
