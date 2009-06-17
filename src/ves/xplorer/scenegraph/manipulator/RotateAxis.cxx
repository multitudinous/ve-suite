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
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/PolygonStipple>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateAxis::RotateAxis( Manipulator* parentManipulator )
    :
    Dragger( parentManipulator )
{
    m_transformationType = TransformationType::ROTATE_AXIS;

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
void RotateAxis::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) )
    {
        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
}
////////////////////////////////////////////////////////////////////////////////
const char* RotateAxis::className() const
{
    return "RotateAxis";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateAxis::clone( const osg::CopyOp& copyop ) const
{
    return new RotateAxis( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateAxis::cloneType() const
{
    return new RotateAxis( m_parentManipulator );
}
////////////////////////////////////////////////////////////////////////////////
bool RotateAxis::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const RotateAxis* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* RotateAxis::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void RotateAxis::SetupDefaultGeometry()
{
    size_t numSegments( 100 );
    double radius( 1.00 );
    double TWO_PI( 2.0 * osg::PI );
    double ringDelta( TWO_PI / numSegments );

    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //Create the rotation axis with line loops
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        for( size_t i = 0; i < numSegments; ++i )
        {
            double rot( i * ringDelta );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double y( radius * cosVal );
            double z( radius * sinVal );

            vertices->push_back( osg::Vec3( 0.0, y, z ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::LINE_LOOP, 0, vertices->size() ) );

        geode->addDrawable( geometry.get() );

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

    //Create invisible torus for picking the rotation axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        size_t numSides( 8 );
        size_t numVerticesPerSegment = 2 * ( numSides + 1 );

        double minorRadius( 0.025 );
        double theta( 0.0 );
        double cosTheta( 1.0 );
        double sinTheta( 0.0 );
        double sideDelta( TWO_PI / numSides );
        for( size_t i = 0; i < numSegments; ++i )
        {
            double phi( 0.0 );
            double newTheta( theta + ringDelta );
            double newCosTheta( cos( newTheta ) );
            double newSinTheta( sin( newTheta ) );
            for( size_t j = 0; j <= numSides; ++j )
            {
                phi += sideDelta;
                double cosPhi( cos( phi ) );
                double sinPhi( sin( phi ) );
                double dist( radius + minorRadius * cosPhi );

                vertices->push_back(
                    osg::Vec3(
                        minorRadius * sinPhi,
                        newCosTheta * dist,
                       -newSinTheta * dist ) );
                vertices->push_back(
                    osg::Vec3(
                        minorRadius * sinPhi,
                        cosTheta * dist,
                        -sinTheta * dist ) );
            }

            theta = newTheta;
            cosTheta = newCosTheta;
            sinTheta = newSinTheta;
        }

        geometry->setVertexArray( vertices.get() );
        for( size_t i = 0; i < numSegments; ++i )
        {
            geometry->addPrimitiveSet(
                new osg::DrawArrays(
                    osg::PrimitiveSet::TRIANGLE_STRIP,
                    i * numVerticesPerSegment,
                    numVerticesPerSegment ) );
        }

        SetDrawableToAlwaysCull( *geometry.get() );
        geode->addDrawable( geometry.get() );
    }

    //Create stippled geometry to show rotation about the axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        vertices->push_back( osg::Vec3( 0.0, 0.0, 0.0 ) );
        for( size_t i = 0; i <= numSegments; ++i )
        {
            double rot( i * ringDelta );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double y( radius * cosVal );
            double z( radius * sinVal );

            vertices->push_back( osg::Vec3( 0.0, y, z ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_FAN, 0, vertices->size() ) );

        //geode->addDrawable( geometry.get() );
        
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

    //Add rotation axis to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
