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
#include <ves/xplorer/scenegraph/manipulator/TranslateAxis.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/ManipulatorRoot.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>
#include <osg/LineSegment>

#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis()
    :
    Dragger(),
    m_unitAxis(
        new osg::LineSegment(
            osg::Vec3d( 0.0, 0.0, 0.0 ), osg::Vec3d( 1.0, 0.0, 0.0 ) ) ),
    m_lineAndCylinderGeode( NULL ),
    m_cone( NULL )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis(
    const TranslateAxis& translateAxis, const osg::CopyOp& copyop )
    :
    Dragger( translateAxis, copyop ),
    m_unitAxis( translateAxis.m_unitAxis.get() ),
    m_lineAndCylinderGeode( translateAxis.m_lineAndCylinderGeode.get() ),
    m_cone( translateAxis.m_cone.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::~TranslateAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::ComputeProjectedPoint(
    const osgUtil::LineSegmentIntersector& deviceInput,
    osg::Vec3d& projectedPoint )
{
    //Transform the dragger axis to world space
    osg::ref_ptr< osg::LineSegment > draggerAxis = new osg::LineSegment();
    draggerAxis->mult( *m_unitAxis, m_localToWorld );
    //draggerAxis->mult( m_localToWorld, *m_unitAxis );

    //Get the start and end points for the dragger axis in world space
    const osg::Vec3d& startDraggerAxis = draggerAxis->start();
    const osg::Vec3d& endDraggerAxis = draggerAxis->end();

    //Get the near and far points for the active device
    const osg::Vec3d& startDeviceInput = deviceInput.getStart();
    const osg::Vec3d& endDeviceInput = deviceInput.getEnd();

    std::cout << "startDraggerAxis: " << "( "
              << startDraggerAxis.x() << ", "
              << startDraggerAxis.y() << ", "
              << startDraggerAxis.z() << " )"
              << std::endl;

    std::cout << "endDraggerAxis: " << "( "
              << endDraggerAxis.x() << ", "
              << endDraggerAxis.y() << ", "
              << endDraggerAxis.z() << " )"
              << std::endl;

    std::cout << "startDeviceInput: " << "( "
              << startDeviceInput.x() << ", "
              << startDeviceInput.y() << ", "
              << startDeviceInput.z() << " )"
              << std::endl;

    std::cout << "endDeviceInput: " << "( "
              << endDeviceInput.x() << ", "
              << endDeviceInput.y() << ", "
              << endDeviceInput.z() << " )"
              << std::endl;

    osg::Vec3d u = endDraggerAxis - startDraggerAxis;
    u.normalize();
    osg::Vec3d v = endDeviceInput - startDeviceInput;
    v.normalize();
    osg::Vec3d w = startDraggerAxis - startDeviceInput;

    double a = u * u;
    double b = u * v;
    double c = v * v;
    double d = u * w;
    double e = v * w;

    double denominator = a * c - b * b;
    if( denominator == 0.0 )
    {
        //If lines are parallel, return
        return;
    }

    double sc = ( b * e - c * d ) / denominator;
    //double tc = ( a * e - b * d ) / denominator;

    projectedPoint = startDraggerAxis + u * sc;
    //osg::Vec3d p2 = startDeviceInput + v * tc;

    //projectedPoint = projectedPoint * m_worldToLocal;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* const TranslateAxis::GetLineAndCylinderGeode() const
{
    return m_lineAndCylinderGeode.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Cone* const TranslateAxis::GetCone() const
{
    return m_cone.get();
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::ManipFunction( const osgUtil::LineSegmentIntersector& deviceInput )
{
    //Get the end projected point
    osg::Vec3d endProjectedPoint;
    ComputeProjectedPoint( deviceInput, endProjectedPoint );

    osg::Vec3d deltaTranslation = endProjectedPoint - m_startProjectedPoint;
    osg::Matrixd motionMatrix = osg::Matrix::translate( deltaTranslation );

    ves::xplorer::scenegraph::ManipulatorRoot* mr =
        ves::xplorer::scenegraph::SceneManager::instance()->GetManipulatorRoot();

    Manipulator* manipulator = mr->GetChild( 0 );

    manipulator->setMatrix( m_startMotionMatrix * motionMatrix );
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::SetupDefaultGeometry()
{
    double cylinderRadius = 0.025;
    double coneRadius = 0.05;
    double coneHeight = 0.2;
    osg::Vec3 coneCenter( coneHeight * 0.25, 0.0, 0.0 );

    //The geode to add the line and cylinder geometry to
    m_lineAndCylinderGeode = new osg::Geode();

    //The geode to add the cone geometry to
    osg::ref_ptr< osg::Geode > coneGeode = new osg::Geode();

    //The unit axis
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->resize( 2 );
    (*vertices)[ 0 ] = osg::Vec3( 0.0, 0.0, 0.0 );
    (*vertices)[ 1 ] = osg::Vec3( 1.0, 0.0, 0.0 );

    //Rotation for cones and cylinders
    osg::Quat rotation;
    rotation.makeRotate( osg::Vec3( 0.0, 0.0, 1.0 ), (*vertices)[ 1 ] );

    //Create a positive line
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        m_lineAndCylinderGeode->addDrawable( geometry.get() );

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

    //Create a positive cone
    {
        (*vertices)[ 1 ].x() -= coneHeight;
        m_cone = new osg::Cone(
            (*vertices)[ 1 ] + coneCenter, coneRadius, coneHeight );
        m_cone->setRotation( rotation );
        osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
            new osg::ShapeDrawable( m_cone.get() );

        coneGeode->addDrawable( shapeDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            shapeDrawable->getOrCreateStateSet();

        //Set line hints
        stateSet->setMode( GL_POLYGON_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create an invisible cylinder for picking the positive line
    {
        osg::ref_ptr< osg::Cylinder > cylinder =
            new osg::Cylinder(
                (*vertices)[ 1 ] * 0.5, cylinderRadius, (*vertices)[ 1 ].x() );
        cylinder->setRotation( rotation );
        osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
            new osg::ShapeDrawable( cylinder.get() );

        SetDrawableToAlwaysCull( *shapeDrawable.get() );
        m_lineAndCylinderGeode->addDrawable( shapeDrawable.get() );
    }


    //Add line and invisible cylinder to this
    addChild( m_lineAndCylinderGeode.get() );

    //Add cone to this
    addChild( coneGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
