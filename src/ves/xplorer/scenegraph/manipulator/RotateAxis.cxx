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
#include <ves/xplorer/scenegraph/manipulator/HelpCircle.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/ClipNode>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateAxis::RotateAxis()
    :
    Rotate( TransformationType::ROTATE_AXIS )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
RotateAxis::RotateAxis(
    const RotateAxis& rotateAxis, const osg::CopyOp& copyop )
    :
    Rotate( rotateAxis, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateAxis::~RotateAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateAxis* RotateAxis::AsRotateAxis()
{
    return this;
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
    return new RotateAxis();
}
////////////////////////////////////////////////////////////////////////////////
const double& RotateAxis::GetRadius() const
{
    return ROTATE_AXIS_RADIUS;
}
////////////////////////////////////////////////////////////////////////////////
bool RotateAxis::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const RotateAxis* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void RotateAxis::SetHelpCircle( HelpCircle* const helpCircle )
{
    Rotate::SetHelpCircle( helpCircle );

    if( !m_helpCircle.valid() )
    {
        //Error output
        return;
    }

    osg::ref_ptr< osg::StateSet > stateSet =
        m_rotateGeode->getOrCreateStateSet();
    m_helpCircle->GetClipNode()->setStateSetModes( 
        *stateSet.get(), osg::StateAttribute::ON );
}
////////////////////////////////////////////////////////////////////////////////
void RotateAxis::SetupDefaultGeometry()
{
    //
    Rotate::SetupDefaultGeometry();

    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //Create invisible torus for picking the rotation axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        unsigned int numVerticesPerSegment = 2 * ( NUM_CIRCLE_SIDES + 1 );

        double theta( 0.0 );
        double cosTheta( 1.0 );
        double sinTheta( 0.0 );
        for( size_t i = 0; i < NUM_CIRCLE_SEGMENTS; ++i )
        {
            double phi( 0.0 );
            double newTheta( theta + DELTA_SEGMENT_ANGLE );
            double newCosTheta( cos( newTheta ) );
            double newSinTheta( sin( newTheta ) );
            for( unsigned int j = 0; j <= NUM_CIRCLE_SIDES; ++j )
            {
                phi += DELTA_SIDE_ANGLE;
                double cosPhi( cos( phi ) );
                double sinPhi( sin( phi ) );
                double dist( ROTATE_AXIS_RADIUS + PICK_RADIUS * cosPhi );

                double s = PICK_RADIUS * sinPhi;
                double t1 = newCosTheta * dist;
                double p1 = -newSinTheta * dist;
                double t2 = cosTheta * dist;
                double p2 = -sinTheta * dist;

                vertices->push_back( osg::Vec3( t1, p1, s ) );
                vertices->push_back( osg::Vec3( t2, p2, s ) );
            }

            theta = newTheta;
            cosTheta = newCosTheta;
            sinTheta = newSinTheta;
        }

        geometry->setVertexArray( vertices.get() );
        for( unsigned int i = 0; i < NUM_CIRCLE_SEGMENTS; ++i )
        {
            geometry->addPrimitiveSet(
                new osg::DrawArrays(
                    osg::PrimitiveSet::TRIANGLE_STRIP,
                    i * numVerticesPerSegment,
                    numVerticesPerSegment ) );
        }

        SetDrawableToAlwaysCull( *geometry.get() );
        SetComputeBBCallback( *geometry.get() );
        geode->addDrawable( geometry.get() );
    }

    //Add rotation axis to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
