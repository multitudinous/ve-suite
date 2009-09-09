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
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>
#include <osg/LineStipple>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ScaleAxis::ScaleAxis()
    :
    Dragger( TransformationType::SCALE_AXIS ),
    m_lineVertices( NULL ),
    m_positiveBox( NULL ),
    m_negativeBox( NULL ),
    m_positiveLineGeometry( NULL ),
    m_negativeLineGeometry( NULL ),
    m_positiveBoxDrawable( NULL ),
    m_negativeBoxDrawable( NULL )
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
    m_positiveLineGeometry( scaleAxis.m_positiveLineGeometry.get() ),
    m_negativeLineGeometry( scaleAxis.m_negativeLineGeometry.get() ),
    m_positiveBoxDrawable( scaleAxis.m_positiveBoxDrawable.get() ),
    m_negativeBoxDrawable( scaleAxis.m_negativeBoxDrawable.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis::~ScaleAxis()
{
    ;
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
void ScaleAxis::ExpandLineVertices( const osg::Vec3& expansion )
{
    (*m_lineVertices)[ 0 ] += expansion;
    (*m_lineVertices)[ 1 ] -= expansion;
    m_positiveLineGeometry->dirtyDisplayList();
    m_positiveLineGeometry->dirtyBound();

    (*m_lineVertices)[ 2 ] -= expansion;
    (*m_lineVertices)[ 3 ] += expansion;
    m_negativeLineGeometry->dirtyDisplayList();
    m_negativeLineGeometry->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
bool ScaleAxis::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const ScaleAxis* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

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

        geode->addDrawable( m_positiveLineGeometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_positiveLineGeometry->getOrCreateStateSet();

        //Override color uniform
        stateSet->addUniform(
            new osg::Uniform( "color", GetColor( Color::DISABLED ) ) );

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

        //Set line stipple
        osg::ref_ptr< osg::LineStipple > lineStipple = new osg::LineStipple();
        lineStipple->setFactor( 1 );
        lineStipple->setPattern( 0xAAAA );
        stateSet->setAttributeAndModes( lineStipple.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create a positive box
    {
        osg::Vec3d BOX_CENTER = unitAxis * BOX_WIDTH;
        (*m_lineVertices)[ 1 ] -= BOX_CENTER;
        BOX_CENTER *= 0.5;
        m_positiveBox = new osg::Box(
            (*m_lineVertices)[ 1 ] + BOX_CENTER, BOX_WIDTH );

        m_positiveBoxDrawable = new osg::ShapeDrawable( m_positiveBox.get() );
        geode->addDrawable( m_positiveBoxDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_positiveBoxDrawable->getOrCreateStateSet();

        //Set polygon hints
        stateSet->setMode( GL_POLYGON_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    /*
    //Create an invisible cylinder for picking the positive line
    {
        osg::ref_ptr< osg::Cylinder > cylinder =
            new osg::Cylinder(
                (*m_lineVertices)[ 1 ] * 0.5,
                PICK_RADIUS,
                (*m_lineVertices)[ 1 ].length() );

        osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
            new osg::ShapeDrawable( cylinder.get() );

        SetDrawableToAlwaysCull( *shapeDrawable.get() );
        geode->addDrawable( shapeDrawable.get() );
    }
    */

    //Create a negative line
    {
        m_negativeLineGeometry = new osg::Geometry();

        m_negativeLineGeometry->setVertexArray( m_lineVertices.get() );
        m_negativeLineGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 2, 2 ) );

        geode->addDrawable( m_negativeLineGeometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_negativeLineGeometry->getOrCreateStateSet();

        //Override color uniform
        stateSet->addUniform(
            new osg::Uniform( "color", GetColor( Color::DISABLED ) ) );

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

        //Set line stipple
        osg::ref_ptr< osg::LineStipple > lineStipple = new osg::LineStipple();
        lineStipple->setFactor( 1 );
        lineStipple->setPattern( 0xAAAA );
        stateSet->setAttributeAndModes( lineStipple.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create a negative box
    {
        osg::Vec3d BOX_CENTER = -unitAxis * BOX_WIDTH;
        (*m_lineVertices)[ 3 ] -= BOX_CENTER;
        BOX_CENTER *= 0.5;
        m_negativeBox = new osg::Box(
            (*m_lineVertices)[ 3 ] + BOX_CENTER, -BOX_WIDTH );

        m_negativeBoxDrawable = new osg::ShapeDrawable( m_negativeBox.get() );
        geode->addDrawable( m_negativeBoxDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_negativeBoxDrawable->getOrCreateStateSet();

        //Set polygon hints
        stateSet->setMode( GL_POLYGON_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    /*
    //Create an invisible cylinder for picking the negative line
    {
        osg::ref_ptr< osg::Cylinder > cylinder =
            new osg::Cylinder(
                (*m_lineVertices)[ 3 ] * 0.5,
                PICK_RADIUS,
                -(*m_lineVertices)[ 3 ].length() );

        osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
            new osg::ShapeDrawable( cylinder.get() );

        SetDrawableToAlwaysCull( *shapeDrawable.get() );
        geode->addDrawable( shapeDrawable.get() );
    }
    */

    //Add lines and cones to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
