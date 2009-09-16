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
#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>
#include <ves/xplorer/scenegraph/manipulator/HelpCircle.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateTwist::RotateTwist()
    :
    Rotate( TransformationType::ROTATE_TWIST )
{
    //If desktop mode
    //setAutoRotateMode( osg::AutoTransform::ROTATE_TO_SCREEN );
    //If cave mode
    setAutoRotateMode( osg::AutoTransform::ROTATE_TO_CAMERA );

    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist::RotateTwist(
    const RotateTwist& rotateTwist, const osg::CopyOp& copyop )
    :
    Rotate( rotateTwist, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist::~RotateTwist()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist* RotateTwist::AsRotateTwist()
{
    return this;
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
    return new RotateTwist();
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::CustomPushAction()
{
    Rotate::CustomPushAction();

    SetLineEndPoint( m_startProjectedPoint );
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::CustomDragAction()
{
    Rotate::CustomDragAction();

    SetLineEndPoint( m_endProjectedPoint );
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::CustomReleaseAction()
{
    Rotate::CustomReleaseAction();

    //Turn ghost disk geode off
    m_lineGeode->setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
bool RotateTwist::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const RotateTwist* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::SetLineEndPoint( const osg::Vec3& endPoint )
{
    (*m_lineVertices)[ 1 ] = endPoint * m_worldToLocal;
    (*m_lineVertices)[ 1 ].normalize();
    (*m_lineVertices)[ 1 ] *= ROTATE_TWIST_RADIUS;

    m_lineGeode->setNodeMask( 1 );
    m_lineGeometry->dirtyDisplayList();
    m_lineGeometry->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //The geode to add the line geometry to
    m_lineGeode = new osg::Geode();

    //The unit axis
    m_lineVertices = new osg::Vec3Array();
    m_lineVertices->resize( 2 );
    (*m_lineVertices)[ 0 ] = osg::Vec3d( 0.0, 0.0, 0.0 );
    (*m_lineVertices)[ 1 ] =  osg::Vec3d( 0.0, 0.0, 0.0 );

    //Create a line
    {
        m_lineGeometry = new osg::Geometry();

        m_lineGeometry->setVertexArray( m_lineVertices.get() );
        m_lineGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        m_lineGeode->addDrawable( m_lineGeometry.get() );
        m_lineGeode->setNodeMask( 0 );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_lineGeometry->getOrCreateStateSet();

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( LINE_WIDTH );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //Create invisible triangle strip for picking the rotation twist axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        double innerRadius( ROTATE_TWIST_RADIUS - PICK_RADIUS );
        double outerRadius( ROTATE_TWIST_RADIUS + PICK_RADIUS );
        for( unsigned int i = 0; i <= NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double si( innerRadius * cosVal );
            double ti( innerRadius * sinVal );

            double so( outerRadius * cosVal );
            double to( outerRadius * sinVal );

            vertices->push_back( osg::Vec3( si, ti, 0.0 ) );
            vertices->push_back( osg::Vec3( so, to, 0.0 ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_STRIP, 0, vertices->size() ) );

        SetDrawableToAlwaysCull( *geometry.get() );
        geode->addDrawable( geometry.get() );
    }

    //Add rotation axis to the scene
    addChild( geode.get() );

    //Add line to this
    addChild( m_lineGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
