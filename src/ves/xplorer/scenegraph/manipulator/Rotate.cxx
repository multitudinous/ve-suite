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

#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/BlendFunc>

// --- STL Includes --- //
#include <cmath>
#include <iostream>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
Rotate::Rotate( const TransformationType::Enum& transformationType )
    :
    Dragger( transformationType )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Rotate::Rotate( const Rotate& rotateAxis, const osg::CopyOp& copyop )
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
void Rotate::CreateGhostDisk()
{
    //Get the start disk point in local coordinates
    osg::Vec3d diskStart = m_startPlaneIntersection * m_worldToLocal;
    diskStart.normalize();
    diskStart *= ROTATE_AXIS_RADIUS;
    (*m_ghostDiskVertices)[ 1 ] = diskStart;

    //Get the end disk point in local coordinates
    osg::Vec3d diskEnd = m_endPlaneIntersection * m_worldToLocal;
    diskEnd.normalize();
    diskEnd *= ROTATE_AXIS_RADIUS;
    (*m_ghostDiskVertices)[ NUM_GHOST_DISK_SEGMENTS + 1 ] = diskEnd;

    //Get the start angle, total angle of rotation, and delta segment rotation
    double angle =
        SignedAngle( osg::Vec3d( 1.0, 0.0, 0.0 ), diskStart, GetUnitAxis() );
    double totalAngle = SignedAngle( diskStart, diskEnd, GetUnitAxis() );
    const double deltaSegmentAngle = totalAngle / NUM_GHOST_DISK_SEGMENTS;

    //Set remaining ghost disk vertices
    for( unsigned int i = 2; i <= NUM_GHOST_DISK_SEGMENTS; ++i )
    {
        angle += deltaSegmentAngle;
        double cosVal = cos( angle );
        double sinVal = sin( angle );
        double s = ROTATE_AXIS_RADIUS * cosVal;
        double t = ROTATE_AXIS_RADIUS * sinVal;
        (*m_ghostDiskVertices)[ i ].set( s, t, 0.0 );
    }

    //Reset the ghost disk geometry
    m_ghostDiskGeometry->dirtyDisplayList();
    m_ghostDiskGeometry->dirtyBound();
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
    //Get the origin in world space
    const osg::Vec3d origin = m_localToWorld.getTrans();

    //Get the projected vectors
    const osg::Vec3d startProject = m_startProjectedPoint - origin;
    const osg::Vec3d endProject = m_endProjectedPoint - origin;

    //Get the plane vectors
    const osg::Vec3d startPlane = m_startPlaneIntersection - origin;
    const osg::Vec3d endPlane = m_endPlaneIntersection - origin;

    //Get the delta angle
    const osg::Vec3d axis = startPlane ^ endPlane;
    double angle = SignedAngle( startProject, endProject, axis );
    m_deltaRotation.makeRotate( angle, axis );
}
////////////////////////////////////////////////////////////////////////////////
const bool Rotate::ComputeProjectedPoint(
    const osgUtil::LineSegmentIntersector& deviceInput,
    osg::Vec3d& projectedPoint )
{
    //Get the near and far points for the active device
    const osg::Vec3d& lineStart = deviceInput.getStart();
    const osg::Vec3d& lineEnd = deviceInput.getEnd();

    //Get intersection with selection line and screen/eye aligned plane
    osg::Plane plane;
    if( m_sceneManager.IsDesktopMode() )
    {
        plane = GetPlane( true );
    }
    else
    {
        plane = GetPlane( false );
    }
    GetLinePlaneIntersection( lineStart, lineEnd, plane, projectedPoint );

    //deviceInput.getFirstIntersection().getWorldIntersectPoint();
    //Project the intersection onto the selected plane
    const osg::Vec3d origin = m_localToWorld.getTrans();
    m_endPlaneIntersection =
        ProjectPointOntoPlane( projectedPoint, origin, GetPlane().getNormal() );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::CustomPushAction()
{
    m_helpCircle->Show();

    m_startPlaneIntersection = m_endPlaneIntersection;

    SetLineEndPoint( m_startProjectedPoint );

    CreateGhostDisk();

    //Turn line geode on
    m_lineGeode->setNodeMask( 1 );

    //Turn ghost disk geode on
    m_ghostDiskGeode->setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::CustomDragAction()
{
    SetLineEndPoint( m_endProjectedPoint );

    CreateGhostDisk();
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::CustomReleaseAction()
{
    m_startPlaneIntersection.set( 0.0, 0.0, 0.0 );
    m_endPlaneIntersection.set( 0.0, 0.0, 0.0 );

    //Turn line geode off
    m_lineGeode->setNodeMask( 0 );

    //Turn ghost disk geode off
    m_ghostDiskGeode->setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::SetLineEndPoint( const osg::Vec3& endPoint )
{
    (*m_lineVertices)[ 1 ] = endPoint * m_worldToLocal;
    (*m_lineVertices)[ 1 ].normalize();
    (*m_lineVertices)[ 1 ] *= GetRadius();

    m_lineGeometry->dirtyDisplayList();
    m_lineGeometry->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void Rotate::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    m_rotateGeode = new osg::Geode();

    //The geode to add the line geometry to
    m_lineGeode = new osg::Geode();

    //The geode to add the ghost disk to
    m_ghostDiskGeode = new osg::Geode();

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
        stateSet->setRenderBinDetails( 11, std::string( "RenderBin" ) );

        //Override color uniform
        stateSet->addUniform(
            new osg::Uniform( "color", osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) ) );

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( LINE_WIDTH );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //Create the rotation twist axis with line loops
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        for( unsigned int i = 0; i < NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double s( GetRadius() * cosVal );
            double t( GetRadius() * sinVal );

            vertices->push_back( osg::Vec3( s, t, 0.0 ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::LINE_LOOP, 0, vertices->size() ) );

        m_rotateGeode->addDrawable( geometry.get() );

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
    addChild( m_rotateGeode.get() );

    //Add line to this
    addChild( m_lineGeode.get() );

    //Add ghost disk to this
    addChild( m_ghostDiskGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
