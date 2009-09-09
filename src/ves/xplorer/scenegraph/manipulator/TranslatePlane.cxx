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
#include <ves/xplorer/scenegraph/manipulator/TranslatePlane.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TranslatePlane::TranslatePlane()
    :
    Dragger( TransformationType::TRANSLATE_PLANE )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
TranslatePlane::TranslatePlane(
    const TranslatePlane& translatePan, const osg::CopyOp& copyop )
    :
    Dragger( translatePan, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslatePlane::~TranslatePlane()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
const char* TranslatePlane::className() const
{
    return "TranslatePlane";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslatePlane::clone( const osg::CopyOp& copyop ) const
{
    return new TranslatePlane( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslatePlane::cloneType() const
{
    return new TranslatePlane();
}
////////////////////////////////////////////////////////////////////////////////
void TranslatePlane::ComputeDeltaTransform()
{
    //Calculate the delta transform
    m_deltaTranslation = m_endProjectedPoint - m_startProjectedPoint;

    //Set the transform
    osg::Vec3d newTranslation =
        m_rootDragger->getPosition() + m_deltaTranslation;
    m_rootDragger->setPosition( newTranslation );
}
////////////////////////////////////////////////////////////////////////////////
//See http://softsurfer.com/Archive/algorithm_0106/algorithm_0106.htm
const bool TranslatePlane::ComputeProjectedPoint(
    const osgUtil::LineSegmentIntersector& deviceInput,
    osg::Vec3d& projectedPoint )
{
    //Get the near and far points for the active device
    const osg::Vec3d& lineStart = deviceInput.getStart();
    const osg::Vec3d& lineEnd = deviceInput.getEnd();

    //Exit if the intersection is invalid
    double intersectDistance;
    if( !GetLinePlaneIntersection(
            lineStart, lineEnd, GetPlane(), projectedPoint ) )
    {
        return false;
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool TranslatePlane::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const TranslatePlane* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void TranslatePlane::SetupDefaultGeometry()
{
    //The geode to add the line and cylinder geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //
    const double innerDistance = 0.45;
    const double outerDistance = 0.55;
    m_triangleVertices = new osg::Vec3Array();
    m_triangleVertices->resize( 16 );
    (*m_triangleVertices)[  0 ] = osg::Vec3d( 0.0,  innerDistance, 0.0 );
    (*m_triangleVertices)[  1 ] = osg::Vec3d(  innerDistance, 0.0, 0.0 );
    (*m_triangleVertices)[  2 ] = osg::Vec3d( 0.0,  outerDistance, 0.0 );
    (*m_triangleVertices)[  3 ] = osg::Vec3d(  outerDistance, 0.0, 0.0 );

    (*m_triangleVertices)[  4 ] = osg::Vec3d(  innerDistance, 0.0, 0.0 );
    (*m_triangleVertices)[  5 ] = osg::Vec3d( 0.0, -innerDistance, 0.0 );
    (*m_triangleVertices)[  6 ] = osg::Vec3d(  outerDistance, 0.0, 0.0 );
    (*m_triangleVertices)[  7 ] = osg::Vec3d( 0.0, -outerDistance, 0.0 );

    (*m_triangleVertices)[  8 ] = osg::Vec3d( 0.0, -innerDistance, 0.0 );
    (*m_triangleVertices)[  9 ] = osg::Vec3d( -innerDistance, 0.0, 0.0 );
    (*m_triangleVertices)[ 10 ] = osg::Vec3d( 0.0, -outerDistance, 0.0 );
    (*m_triangleVertices)[ 11 ] = osg::Vec3d( -outerDistance, 0.0, 0.0 );

    (*m_triangleVertices)[ 12 ] = osg::Vec3d( -innerDistance, 0.0, 0.0 );
    (*m_triangleVertices)[ 13 ] = osg::Vec3d( 0.0,  innerDistance, 0.0 );
    (*m_triangleVertices)[ 14 ] = osg::Vec3d( -outerDistance, 0.0, 0.0 );
    (*m_triangleVertices)[ 15 ] = osg::Vec3d( 0.0,  outerDistance, 0.0 );

    //Create a triangle
    {
        m_triangleGeometry = new osg::Geometry();

        m_triangleGeometry->setVertexArray( m_triangleVertices.get() );
        m_triangleGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::TRIANGLE_STRIP, 0, 4 ) );

        geode->addDrawable( m_triangleGeometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_triangleGeometry->getOrCreateStateSet();
    }

    //Create a triangle
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();

        geometry->setVertexArray( m_triangleVertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::TRIANGLE_STRIP, 4, 4 ) );

        geode->addDrawable( geometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            geometry->getOrCreateStateSet();
    }

    //Create a triangle
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();

        geometry->setVertexArray( m_triangleVertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::TRIANGLE_STRIP, 8, 4 ) );

        geode->addDrawable( geometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            geometry->getOrCreateStateSet();
    }

    //Create a triangle
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();

        geometry->setVertexArray( m_triangleVertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::TRIANGLE_STRIP, 12, 4 ) );

        geode->addDrawable( geometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            geometry->getOrCreateStateSet();
    }

    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
