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

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>
#include <osg/LineSegment>

// --- C/C++ Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis()
    :
    Dragger( TransformationType::TRANSLATE_AXIS ),
    m_explodeDistance( TRANSLATE_PAN_RADIUS ),
    m_lineVertices( NULL ),
    m_positiveCone( NULL ),
    m_negativeCone( NULL ),
    m_positiveCylinder( NULL ),
    m_negativeCylinder( NULL ),
    m_positiveLineGeometry( NULL ),
    m_negativeLineGeometry( NULL ),
    m_positiveConeDrawable( NULL ),
    m_negativeConeDrawable( NULL ),
    m_positiveCylinderDrawable( NULL ),
    m_negativeCylinderDrawable( NULL ),
    m_lineAndCylinderGeode( NULL )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis(
    const TranslateAxis& translateAxis, const osg::CopyOp& copyop )
    :
    Dragger( translateAxis, copyop ),
    m_explodeDistance( translateAxis.m_explodeDistance ),
    m_lineVertices( translateAxis.m_lineVertices.get() ),
    m_positiveCone( translateAxis.m_positiveCone.get() ),
    m_negativeCone( translateAxis.m_negativeCone.get() ),
    m_positiveCylinder( translateAxis.m_positiveCylinder.get() ),
    m_negativeCylinder( translateAxis.m_negativeCylinder.get() ),
    m_positiveLineGeometry( translateAxis.m_positiveLineGeometry.get() ),
    m_negativeLineGeometry( translateAxis.m_negativeLineGeometry.get() ),
    m_positiveConeDrawable( translateAxis.m_positiveConeDrawable.get() ),
    m_negativeConeDrawable( translateAxis.m_negativeConeDrawable.get() ),
    m_positiveCylinderDrawable( translateAxis.m_positiveCylinderDrawable.get() ),
    m_negativeCylinderDrawable( translateAxis.m_negativeCylinderDrawable.get() ),
    m_lineAndCylinderGeode( translateAxis.m_lineAndCylinderGeode.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::~TranslateAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis* TranslateAxis::AsTranslateAxis()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
const char* TranslateAxis::className() const
{
    return "TranslateAxis";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslateAxis::clone( const osg::CopyOp& copyop ) const
{
    return new TranslateAxis( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslateAxis::cloneType() const
{
    return new TranslateAxis();
}
////////////////////////////////////////////////////////////////////////////////
bool TranslateAxis::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const TranslateAxis* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::ComboForm()
{
    if( m_comboForm )
    {
        return;
    }

    //Call base method
    Dragger::ComboForm();

    osg::Vec3d explodeVector = GetUnitAxis() * m_explodeDistance;

    //Move the line in from the origin and unit axis
    (*m_lineVertices)[ 0 ] += explodeVector;
    m_positiveLineGeometry->dirtyDisplayList();
    m_positiveLineGeometry->dirtyBound();

    (*m_lineVertices)[ 2 ] -= explodeVector;
    m_negativeLineGeometry->dirtyDisplayList();
    m_negativeLineGeometry->dirtyBound();

    //Move the invisible cylinder to match the new line position
    m_positiveCylinder->setCenter(
        m_positiveCylinder->getCenter() + ( explodeVector * 0.5 ) );
    m_positiveCylinder->setHeight(
        m_positiveCylinder->getHeight() - TRANSLATE_PAN_RADIUS );

    m_positiveCylinderDrawable->dirtyDisplayList();
    m_positiveCylinderDrawable->dirtyBound();

    m_negativeCylinder->setCenter(
        m_negativeCylinder->getCenter() - ( explodeVector * 0.5 ) );
    m_negativeCylinder->setHeight(
        m_negativeCylinder->getHeight() + TRANSLATE_PAN_RADIUS );

    m_negativeCylinderDrawable->dirtyDisplayList();
    m_negativeCylinderDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::DefaultForm()
{
    if( !m_comboForm )
    {
        return;
    }

    //Call base method
    Dragger::DefaultForm();

    osg::Vec3d explodeVector = GetUnitAxis() * m_explodeDistance;

    //Move the line back to the origin and unit axis
    (*m_lineVertices)[ 0 ] -= explodeVector;
    m_positiveLineGeometry->dirtyDisplayList();
    m_positiveLineGeometry->dirtyBound();

    (*m_lineVertices)[ 2 ] += explodeVector;
    m_negativeLineGeometry->dirtyDisplayList();
    m_negativeLineGeometry->dirtyBound();

    //Move the invisible cylinder to match the new line position
    m_positiveCylinder->setCenter(
        m_positiveCylinder->getCenter() - ( explodeVector * 0.5 ) );
    m_positiveCylinder->setHeight(
        m_positiveCylinder->getHeight() + TRANSLATE_PAN_RADIUS );

    m_positiveCylinderDrawable->dirtyDisplayList();
    m_positiveCylinderDrawable->dirtyBound();

    m_negativeCylinder->setCenter(
        m_negativeCylinder->getCenter() + ( explodeVector * 0.5 ) );
    m_negativeCylinder->setHeight(
        m_negativeCylinder->getHeight() - TRANSLATE_PAN_RADIUS );

    m_negativeCylinderDrawable->dirtyDisplayList();
    m_negativeCylinderDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::EnableLinesAndCylinders( const bool& enable )
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
void TranslateAxis::ComputeDeltaTransform()
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
const bool TranslateAxis::ComputeProjectedPoint(
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
void TranslateAxis::ConeCenterOffset( const osg::Vec3& offset )
{
    m_positiveCone->setCenter( m_positiveCone->getCenter() + offset );
    m_positiveConeDrawable->dirtyDisplayList();
    m_positiveConeDrawable->dirtyBound();

    m_negativeCone->setCenter( m_negativeCone->getCenter() - offset );
    m_negativeConeDrawable->dirtyDisplayList();
    m_negativeConeDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::SetupDefaultGeometry()
{
    //The geode to add the line and cylinder geometry to
    m_lineAndCylinderGeode = new osg::Geode();

    //The geode to add the cone geometry to
    osg::ref_ptr< osg::Geode > coneGeode = new osg::Geode();

    //The unit axis
    const osg::Vec3d unitAxis = GetUnitAxis();
    m_lineVertices = new osg::Vec3Array();
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

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( LINE_WIDTH );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //Create a positive cone
    {
        osg::Vec3d CONE_CENTER = unitAxis * CONE_HEIGHT;
        (*m_lineVertices)[ 1 ] -= CONE_CENTER;
        CONE_CENTER *= 0.25;
        m_positiveCone = new osg::Cone(
            (*m_lineVertices)[ 1 ] + CONE_CENTER, CONE_RADIUS, CONE_HEIGHT );

        m_positiveConeDrawable = new osg::ShapeDrawable( m_positiveCone.get() );
        coneGeode->addDrawable( m_positiveConeDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_positiveConeDrawable->getOrCreateStateSet();
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

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( LINE_WIDTH );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //Create a negative cone
    {
        osg::Vec3d CONE_CENTER = -unitAxis * CONE_HEIGHT;
        (*m_lineVertices)[ 3 ] -= CONE_CENTER;
        CONE_CENTER *= 0.25;
        m_negativeCone = new osg::Cone(
            (*m_lineVertices)[ 3 ] + CONE_CENTER, CONE_RADIUS, -CONE_HEIGHT );

        m_negativeConeDrawable = new osg::ShapeDrawable( m_negativeCone.get() );
        //coneGeode->addDrawable( m_negativeConeDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_negativeConeDrawable->getOrCreateStateSet();
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

    //Add line and invisible cylinder to this
    addChild( m_lineAndCylinderGeode.get() );

    //Add cones to this
    addChild( coneGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
