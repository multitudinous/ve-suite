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
#include <ves/xplorer/scenegraph/manipulator/ScaleAxis.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ScaleAxis::ScaleAxis()
    :
    Dragger( TransformationType::SCALE_AXIS ),
    m_lineVertices( NULL ),
    m_positiveBox( NULL ),
    m_negativeBox( NULL ),
    m_positiveCylinder( NULL ),
    m_negativeCylinder( NULL ),
    m_positiveLineGeometry( NULL ),
    m_negativeLineGeometry( NULL ),
    m_positiveBoxDrawable( NULL ),
    m_negativeBoxDrawable( NULL ),
    m_positiveCylinderDrawable( NULL ),
    m_negativeCylinderDrawable( NULL ),
    m_lineAndCylinderGeode( NULL )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis::ScaleAxis(
    const ScaleAxis& scaleAxis, const osg::CopyOp& copyop )
    :
    Dragger( scaleAxis, copyop ),
    m_lineVertices( scaleAxis.m_lineVertices.get() ),
    m_positiveBox( scaleAxis.m_positiveBox.get() ),
    m_negativeBox( scaleAxis.m_negativeBox.get() ),
    m_positiveCylinder( scaleAxis.m_positiveCylinder.get() ),
    m_negativeCylinder( scaleAxis.m_negativeCylinder.get() ),
    m_positiveLineGeometry( scaleAxis.m_positiveLineGeometry.get() ),
    m_negativeLineGeometry( scaleAxis.m_negativeLineGeometry.get() ),
    m_positiveBoxDrawable( scaleAxis.m_positiveBoxDrawable.get() ),
    m_negativeBoxDrawable( scaleAxis.m_negativeBoxDrawable.get() ),
    m_positiveCylinderDrawable( scaleAxis.m_positiveCylinderDrawable.get() ),
    m_negativeCylinderDrawable( scaleAxis.m_negativeCylinderDrawable.get() ),
    m_lineAndCylinderGeode( scaleAxis.m_lineAndCylinderGeode.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis::~ScaleAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis* ScaleAxis::AsScaleAxis()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::BoxCenterOffset( const osg::Vec3& offset )
{
    m_positiveBox->setCenter( m_positiveBox->getCenter() + offset );
    m_positiveBoxDrawable->dirtyDisplayList();
    m_positiveBoxDrawable->dirtyBound();

    m_negativeBox->setCenter( m_negativeBox->getCenter() - offset );
    m_negativeBoxDrawable->dirtyDisplayList();
    m_negativeBoxDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
const char* ScaleAxis::className() const
{
    return "ScaleAxis";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ScaleAxis::clone( const osg::CopyOp& copyop ) const
{
    return new ScaleAxis( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ScaleAxis::cloneType() const
{
    return new ScaleAxis();
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::ComputeDeltaTransform()
{
    const osg::Vec3d origin = m_localToWorld.getTrans();
    const osg::Vec3d axis = GetAxis();

    //Calculate the axis vector and vectors from the translation point
    const osg::Vec3d originToAxis = axis - origin;
    const osg::Vec3d originToStart = m_startProjectedPoint - origin;
    const osg::Vec3d originToEnd = m_endProjectedPoint - origin;

    //
    double scale( 1.0 );
    if( originToStart.x() > 0.0 )
    {
        scale = originToEnd.x() / originToStart.x();
    }
    //const double D = 1.0 / originToAxis.length();
    //const double projS = ( originToStart * originToAxis ) * D;
    //const double projE = ( originToEnd * originToAxis ) * D;


    //Find the ratio between the projected scalar values
    //const double ratio = projE / projS;

    //m_deltaScale.set( scale, scale, scale );

    //Set the transform
    //osg::Vec3d newScale =
        //m_rootDragger->getPosition() + m_deltaTranslation;
    //m_rootDragger->setPosition( newTranslation );
}
////////////////////////////////////////////////////////////////////////////////
//See http://softsurfer.com/Archive/algorithm_0106/algorithm_0106.htm
const bool ScaleAxis::ComputeProjectedPoint(
    const osgUtil::LineSegmentIntersector& deviceInput,
    osg::Vec3d& projectedPoint )
{
    //Get the start and end points for the dragger axis in world space
    const osg::Vec3d startDraggerAxis = m_localToWorld.getTrans();
    const osg::Vec3d endDraggerAxis = GetAxis( true );

    //Get the near and far points for the active device
    const osg::Vec3d& startDeviceInput = deviceInput.getStart();
    const osg::Vec3d& endDeviceInput = deviceInput.getEnd();

    osg::Vec3d u = endDraggerAxis - startDraggerAxis;
    osg::Vec3d v = endDeviceInput - startDeviceInput;
    osg::Vec3d w = startDraggerAxis - startDeviceInput;

    double a = u * u;
    double b = u * v;
    double c = v * v;
    double d = u * w;
    double e = v * w;

    //If the lines are not parallel
    double D = ( a * c ) - ( b * b );
    double sc( 0.0 );
    if( D > 1E-05 )
    {
        sc = ( b * e - c * d ) / D;
    }

    //Compute the line parameters of the two closest points
    projectedPoint = startDraggerAxis + ( u * sc );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::EnableLinesAndCylinders( const bool& enable )
{
    if( enable )
    {
        m_lineAndCylinderGeode->setNodeMask( 1 );
    }
    else
    {
        m_lineAndCylinderGeode->setNodeMask( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::ExpandLineVertices( const osg::Vec3& expansion )
{
    (*m_lineVertices)[ 0 ] += expansion;
    (*m_lineVertices)[ 1 ] -= expansion;
    m_positiveLineGeometry->dirtyDisplayList();
    m_positiveLineGeometry->dirtyBound();

    //m_positiveCylinder->setCenter(
        //m_positiveCylinder->getCenter() + ( expansion * 0.5 ) );
    m_positiveCylinder->setHeight(
        m_positiveCylinder->getHeight() - expansion.length() );

    m_positiveCylinderDrawable->dirtyDisplayList();
    m_positiveCylinderDrawable->dirtyBound();

    (*m_lineVertices)[ 2 ] -= expansion;
    (*m_lineVertices)[ 3 ] += expansion;
    m_negativeLineGeometry->dirtyDisplayList();
    m_negativeLineGeometry->dirtyBound();

    //m_negativeCylinder->setCenter(
        //m_negativeCylinder->getCenter() - ( expansion * 0.5 ) );
    m_negativeCylinder->setHeight(
        m_negativeCylinder->getHeight() + expansion.length() );

    m_negativeCylinderDrawable->dirtyDisplayList();
    m_negativeCylinderDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
bool ScaleAxis::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const ScaleAxis* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::SetupDefaultGeometry()
{
    //The geode to add the line and cylinder geometry to
    m_lineAndCylinderGeode = new osg::Geode();

    //The geode to add the cone geometry to
    osg::ref_ptr< osg::Geode > boxGeode = new osg::Geode();

    //The unit axis
    const osg::Vec3d unitAxis = GetUnitAxis();
    m_lineVertices = new osg::Vec3Array;
    m_lineVertices->resize( 4 );
    (*m_lineVertices)[ 0 ] = osg::Vec3d( 0.0, 0.0, 0.0 );
    (*m_lineVertices)[ 1 ] =  unitAxis;
    (*m_lineVertices)[ 2 ] = osg::Vec3d( 0.0, 0.0, 0.0 );
    (*m_lineVertices)[ 3 ] = -unitAxis;

    //Create a positive line
    {
        m_positiveLineGeometry = new osg::Geometry();

        m_positiveLineGeometry->setVertexArray( m_lineVertices.get() );
        m_positiveLineGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        m_lineAndCylinderGeode->addDrawable( m_positiveLineGeometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_positiveLineGeometry->getOrCreateStateSet();

        //Override color uniform
        //stateSet->addUniform(
            //new osg::Uniform( "color", GetColor( Color::DISABLED ) ) );

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( LINE_WIDTH );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //Create a positive box
    {
        osg::Vec3d BOX_CENTER = unitAxis * BOX_WIDTH;
        (*m_lineVertices)[ 1 ] -= BOX_CENTER;
        BOX_CENTER *= 0.5;
        m_positiveBox = new osg::Box(
            (*m_lineVertices)[ 1 ] + BOX_CENTER, BOX_WIDTH );

        m_positiveBoxDrawable = new osg::ShapeDrawable( m_positiveBox.get() );
        boxGeode->addDrawable( m_positiveBoxDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_positiveBoxDrawable->getOrCreateStateSet();
    }

    //Create an invisible cylinder for picking the positive line
    {
        m_positiveCylinder = new osg::Cylinder(
            (*m_lineVertices)[ 1 ] * 0.5,
            PICK_RADIUS,
            (*m_lineVertices)[ 1 ].length() );

        m_positiveCylinderDrawable =
            new osg::ShapeDrawable( m_positiveCylinder.get() );

        SetDrawableToAlwaysCull( *m_positiveCylinderDrawable.get() );
        m_lineAndCylinderGeode->addDrawable( m_positiveCylinderDrawable.get() );
    }

    //Create a negative line
    {
        m_negativeLineGeometry = new osg::Geometry();

        m_negativeLineGeometry->setVertexArray( m_lineVertices.get() );
        m_negativeLineGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 2, 2 ) );

        //m_lineAndCylinderGeode->addDrawable( m_negativeLineGeometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_negativeLineGeometry->getOrCreateStateSet();

        //Override color uniform
        //stateSet->addUniform(
            //new osg::Uniform( "color", GetColor( Color::DISABLED ) ) );

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( LINE_WIDTH );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //Create a negative box
    {
        osg::Vec3d BOX_CENTER = -unitAxis * BOX_WIDTH;
        (*m_lineVertices)[ 3 ] -= BOX_CENTER;
        BOX_CENTER *= 0.5;
        m_negativeBox = new osg::Box(
            (*m_lineVertices)[ 3 ] + BOX_CENTER, -BOX_WIDTH );

        m_negativeBoxDrawable = new osg::ShapeDrawable( m_negativeBox.get() );
        //boxGeode->addDrawable( m_negativeBoxDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_negativeBoxDrawable->getOrCreateStateSet();
    }

    //Create an invisible cylinder for picking the negative line
    {
        m_negativeCylinder = new osg::Cylinder(
            (*m_lineVertices)[ 3 ] * 0.5,
            PICK_RADIUS,
            -(*m_lineVertices)[ 3 ].length() );

        m_negativeCylinderDrawable =
            new osg::ShapeDrawable( m_negativeCylinder.get() );

        SetDrawableToAlwaysCull( *m_negativeCylinderDrawable.get() );
        //m_lineAndCylinderGeode->addDrawable( m_negativeCylinderDrawable.get() );
    }

    //Add lines and cones to the scene
    addChild( m_lineAndCylinderGeode.get() );

    //Add boxes to this
    addChild( boxGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
