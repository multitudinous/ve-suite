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
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>
#include <osg/LineStipple>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ScaleAxis::ScaleAxis( Manipulator* parentManipulator )
    :
    Dragger( parentManipulator ),
    m_defaultAxisColor( 0.7, 0.7, 0.7, 1.0 ),
    m_axisColor( NULL ),
    m_lineVertices( NULL ),
    m_lineGeometry( NULL ),
    m_box( NULL ),
    m_shapeDrawable( NULL )
{
    m_transformationType = TransformationType::SCALE_AXIS;
    m_axisColor = new osg::Uniform( "color", m_defaultAxisColor );

    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis::ScaleAxis(
    const ScaleAxis& scaleAxis, const osg::CopyOp& copyop )
    :
    Dragger( scaleAxis, copyop ),
    m_defaultAxisColor( scaleAxis.m_defaultAxisColor ),
    m_axisColor( scaleAxis.m_axisColor.get() ),
    m_lineVertices( scaleAxis.m_lineVertices.get() ),
    m_lineGeometry( scaleAxis.m_lineGeometry.get()  ),
    m_box( scaleAxis.m_box.get()  ),
    m_shapeDrawable( scaleAxis.m_shapeDrawable.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis::~ScaleAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) )
    {
        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
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
    return new ScaleAxis( m_parentManipulator );
}
////////////////////////////////////////////////////////////////////////////////
bool ScaleAxis::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const ScaleAxis* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* ScaleAxis::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::DirtyGeometry()
{
    m_lineGeometry->dirtyDisplayList();
    m_lineGeometry->dirtyBound();

    m_shapeDrawable->dirtyDisplayList();
    m_shapeDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
osg::Box* const ScaleAxis::GetBox() const
{
    return m_box.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec3Array* const ScaleAxis::GetLineVertices() const
{
    return m_lineVertices.get();
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //The unit axis
    m_lineVertices = new osg::Vec3Array();
    m_lineVertices->resize( 2 );
    (*m_lineVertices)[ 0 ] = osg::Vec3( 0.0, 0.0, 0.0 );
    (*m_lineVertices)[ 1 ] = osg::Vec3( 1.0, 0.0, 0.0 );

    //Rotation for boxes
    osg::Quat rotation;
    rotation.makeRotate( osg::Vec3( 0.0, 0.0, 1.0 ), (*m_lineVertices)[ 1 ] );

    //Create a positive line
    {
        m_lineGeometry = new osg::Geometry();

        m_lineGeometry->setVertexArray( m_lineVertices.get() );
        m_lineGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        geode->addDrawable( m_lineGeometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_lineGeometry->getOrCreateStateSet();

        //Override color uniform
        stateSet->addUniform( m_axisColor.get() );

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
        osg::Vec3 BOX_CENTER( BOX_WIDTH * 0.5, 0.0, 0.0 );
        (*m_lineVertices)[ 1 ].x() -= BOX_WIDTH;
        m_box = new osg::Box( (*m_lineVertices)[ 1 ] + BOX_CENTER, BOX_WIDTH );
        m_box->setRotation( rotation );

        m_shapeDrawable = new osg::ShapeDrawable( m_box.get() );
        geode->addDrawable( m_shapeDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_shapeDrawable->getOrCreateStateSet();

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
                (*m_lineVertices)[ 1 ] * 0.5, CYLINDER_RADIUS,
                (*m_lineVertices)[ 1 ].x() );
        cylinder->setRotation( rotation );
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
/*
void ScaleAxis::UseColor( ColorTag::Enum colorTag )
{
    osg::Vec4& color = GetColor( colorTag );

    m_color->set( color );
    
    if( colorTag == ColorTag::DEFAULT )
    {
        m_axisColor->set( m_defaultAxisColor );
    }
    else
    {
        m_axisColor->set( color );
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
