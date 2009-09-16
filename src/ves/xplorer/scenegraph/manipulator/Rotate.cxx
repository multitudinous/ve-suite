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
#include <ves/xplorer/scenegraph/manipulator/Rotate.h>
#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>
#include <ves/xplorer/scenegraph/manipulator/HelpCircle.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/BlendFunc>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
Rotate::Rotate(
    const TransformationType::Enum& transformationType )
    :
    Dragger( transformationType )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
Rotate::Rotate(
    const Rotate& rotateAxis, const osg::CopyOp& copyop )
    :
    Dragger( rotateAxis, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Rotate::~Rotate()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Rotate* Rotate::AsRotate()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
const char* Rotate::className() const
{
    return "Rotate";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* Rotate::clone( const osg::CopyOp& copyop ) const
{
    return new Rotate( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* Rotate::cloneType() const
{
    return new Rotate( m_transformationType );
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::CreateGhostDisk()
{
    //Set variables and initial ghost disk vertex
    double rot = m_startAngle;
    double cosVal( 0.0 );
    double sinVal( 0.0 );
    double s( 0.0 );
    double t( 0.0 );
    (*m_ghostDiskVertices)[ 1 ] = m_localStartPoint;

    //Set rest of ghost disk vertices
    const double deltaSegmentAngle = m_angle / NUM_GHOST_DISK_SEGMENTS;
    //std::cout << "deltaSegmentAngle: " << deltaSegmentAngle << std::endl;
    for( unsigned int i = 2; i < m_ghostDiskVertices->size(); ++i )
    {
        rot += deltaSegmentAngle;
        cosVal = cos( rot );
        sinVal = sin( rot );
        s = ROTATE_AXIS_RADIUS * cosVal;
        t = ROTATE_AXIS_RADIUS * sinVal;

        (*m_ghostDiskVertices)[ i ].set( s, t, 0.0 );
    }

    //Turn ghost disk geode on and reset the ghost disk geometry
    m_ghostDiskGeode->setNodeMask( 1 );
    m_ghostDiskGeometry->dirtyDisplayList();
    m_ghostDiskGeometry->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
const HelpCircle* const Rotate::GetHelpCircle() const
{
    return m_helpCircle.get();
}
////////////////////////////////////////////////////////////////////////////////
bool Rotate::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const Rotate* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::SetHelpCircle( HelpCircle* const helpCircle )
{
    if( m_helpCircle != helpCircle )
    {
        m_helpCircle = helpCircle;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::ComputeDeltaTransform()
{
    const osg::Vec3d origin = GetAxis( true, true );

    //Get the direction vectors of the rotation origin to start and end points
    osg::Vec3d originToStart = m_startProjectedPoint - origin;
    originToStart.normalize();
    osg::Vec3d originToEnd = m_endProjectedPoint - origin;
    originToEnd.normalize();

    /*
    osg::Vec3d localPoint = m_endProjectedPoint * m_worldToLocal;
    localPoint.normalize();
    localPoint *= ROTATE_AXIS_RADIUS;
    m_endAngle = acos( m_localStartPoint * localPoint );

    std::cout << "m_endAngle: " << m_endAngle << std::endl;
    */

    //Calculate cross products of the direction vectors with rotation axis
    osg::Vec3d rotationAxis = originToStart ^ originToEnd;
    rotationAxis.normalize();
    osg::Vec3d crossRotStart = rotationAxis ^ originToStart;
    crossRotStart.normalize();
    osg::Vec3d crossRotEnd = rotationAxis ^ originToEnd;
    crossRotEnd.normalize();

    //Calculate the cross product of the above start and end cross products
    osg::Vec3d crossStartEnd = crossRotStart ^ crossRotEnd;
    crossStartEnd.normalize();

    //Dot the two direction vectors and get the arccos of the dot product to get
    //the angle between them, then multiply it by the sign of the dot product
    //of the derived cross product calculated above to obtain the direction
    //by which we should rotate with the angle
    double dot = originToStart * originToEnd;
    //Protect against zero movement
    if( dot < -1.0 || dot > 1.0 )
    {
        return;
    }

    //Create a normalized quaternion representing the rotation from the start to end points
    double rotationAngle;
    switch( m_transformationType )
    {
    case TransformationType::ROTATE_TWIST:
    {
        rotationAngle =
            acos( dot ) * osg::sign( rotationAxis * crossStartEnd );
        m_deltaRotation.makeRotate( rotationAngle, rotationAxis );

        break;
    }
    default:
    {
        rotationAngle =
            acos( dot ) * osg::sign( GetAxis() * crossStartEnd );
        m_deltaRotation.makeRotate( rotationAngle, GetAxis() );
    }
    } //switch( m_transformationType )
    //m_deltaRotation /= m_deltaRotation.length();

    /*
    //Add the calculated rotation to the current rotation
    osg::Quat newRotation = m_deltaRotation * m_rootDragger->getRotation();
    if( m_vectorSpace == VectorSpace::LOCAL )
    {
        m_rootDragger->setRotation( newRotation );
    }
    */

    //std::cout << "rotationAngle: " << rotationAngle << std::endl;
    m_angle += rotationAngle;
}
////////////////////////////////////////////////////////////////////////////////
const bool Rotate::ComputeProjectedPoint(
    const osgUtil::LineSegmentIntersector& deviceInput,
    osg::Vec3d& projectedPoint )
{
    //Get the near and far points for the active device
    const osg::Vec3d& lineStart = deviceInput.getStart();
    const osg::Vec3d& lineEnd = deviceInput.getEnd();

    //Exit if the intersection is invalid
    switch( m_transformationType )
    {
    case TransformationType::ROTATE_TWIST:
    {
        if( !GetLinePlaneIntersection(
                lineStart, lineEnd, GetPlane(), projectedPoint ) )
        {
            return false;
        }

        break;
    }
    default:
    {
        if( !GetLinePlaneIntersection(
                lineStart, lineEnd, GetPlane( true ), projectedPoint ) )
        {
            return false;
        }
    }
    } //switch( m_transformationType )

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::CustomPushAction()
{
    m_helpCircle->Show();

    m_localStartPoint = m_startProjectedPoint * m_worldToLocal;
    m_localStartPoint.normalize();
    m_localStartPoint *= ROTATE_AXIS_RADIUS;
    m_startAngle = acos( osg::Vec3d( 1.0, 0.0, 0.0 ) * m_localStartPoint );

    //std::cout << "m_startAngle: " << m_startAngle << std::endl;
    //std::cout << "m_localStartPoint: " << m_localStartPoint << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::CustomDragAction()
{
    //CreateGhostDisk();
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::CustomReleaseAction()
{
    m_angle = 0.0;

    //Turn ghost disk geode off
    m_ghostDiskGeode->setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    m_ghostDiskGeode = new osg::Geode();

    double radius( 0.0 );
    switch( m_transformationType )
    {
    case TransformationType::ROTATE_TWIST:
    {
        radius = ROTATE_TWIST_RADIUS;

        break;
    }
    default:
    {
        radius = ROTATE_AXIS_RADIUS;
    }
    } //switch( m_transformationType )

    //Create the rotation twist axis with line loops
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        for( unsigned int i = 0; i < NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double s( radius * cosVal );
            double t( radius * sinVal );

            vertices->push_back( osg::Vec3( s, t, 0.0 ) );
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
    }

    //Create geometry to show rotation about the selected axis
    {
        m_ghostDiskGeometry = new osg::Geometry();
        m_ghostDiskVertices = new osg::Vec3Array();
        m_ghostDiskVertices->resize( NUM_GHOST_DISK_SEGMENTS + 2 );

        m_ghostDiskVertices->push_back( osg::Vec3( 0.0, 0.0, 0.0 ) );
        for( unsigned int i = 0; i <= NUM_GHOST_DISK_SEGMENTS; ++i )
        {
            m_ghostDiskVertices->push_back( osg::Vec3( 0.0, 0.0, 0.0 ) );
        }

        m_ghostDiskGeometry->setVertexArray( m_ghostDiskVertices.get() );
        m_ghostDiskGeometry->addPrimitiveSet( new osg::DrawArrays(
            osg::PrimitiveSet::TRIANGLE_FAN, 0, m_ghostDiskVertices->size() ) );

        m_ghostDiskGeode->addDrawable( m_ghostDiskGeometry.get() );
        m_ghostDiskGeode->setNodeMask( 0 );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_ghostDiskGeometry->getOrCreateStateSet();
        osg::ref_ptr< osg::BlendFunc > blendFunc = new osg::BlendFunc();
        blendFunc->setFunction(
            osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
        stateSet->setMode(
            GL_BLEND,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateSet->setAttributeAndModes(
            blendFunc.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        //Override color uniform
        osg::Vec4 color = GetColor( Color::DISABLED );
        color.w() = 0.64;
        stateSet->addUniform( new osg::Uniform( "color", color ) );
    }

    //Add rotation axis to the scene
    addChild( geode.get() );

    addChild( m_ghostDiskGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
