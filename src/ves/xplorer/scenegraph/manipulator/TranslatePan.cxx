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
#include <ves/xplorer/scenegraph/manipulator/TranslatePan.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geometry>
#include <osg/Billboard>
#include <osg/LineWidth>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TranslatePan::TranslatePan()
    :
    Dragger( TransformationType::TRANSLATE_PAN )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
TranslatePan::TranslatePan(
    const TranslatePan& translatePan, const osg::CopyOp& copyop )
    :
    Dragger( translatePan, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslatePan::~TranslatePan()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TranslatePan::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) )
    {
        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
}
////////////////////////////////////////////////////////////////////////////////
const char* TranslatePan::className() const
{
    return "TranslatePan";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslatePan::clone( const osg::CopyOp& copyop ) const
{
    return new TranslatePan( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslatePan::cloneType() const
{
    return new TranslatePan();
}
////////////////////////////////////////////////////////////////////////////////
void TranslatePan::ComputeDeltaTransform()
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
const bool TranslatePan::ComputeProjectedPoint(
    const osgUtil::LineSegmentIntersector& deviceInput,
    osg::Vec3d& projectedPoint )
{
    //Get the start and end points for the dragger axis in world space
    const osg::Vec3d startDraggerAxis = GetUnitAxis( true, true );
    const osg::Vec3d endDraggerAxis = GetUnitAxis( false, true );

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
    
    //Compute the line parameters of the two closest points
    double sc( 0.0 );
    double D = ( a * c ) - ( b * b );
    //If the lines are not parallel
    if( D > 0.00000001 )
    {
        sc = ( b * e - c * d ) / D;
    }

    projectedPoint = startDraggerAxis + ( u * sc );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool TranslatePan::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const TranslatePan* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* TranslatePan::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void TranslatePan::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Billboard > billboard = new osg::Billboard();
    billboard->setMode( osg::Billboard::POINT_ROT_EYE );

    //Create the rotation axis with line loops
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        for( unsigned int i = 0; i < NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double s( TRANSLATE_PAN_RADIUS * cosVal );
            double t( TRANSLATE_PAN_RADIUS * sinVal );

            vertices->push_back( osg::Vec3( s, 0.0, t ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::LINE_LOOP, 0, vertices->size() ) );

        billboard->addDrawable( geometry.get() );

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

    //Create invisible triangle fan to select the translate pan dragger
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        vertices->push_back( osg::Vec3( 0.0, 0.0, 0.0 ) );
        for( unsigned int i = 0; i <= NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double s( TRANSLATE_PAN_RADIUS * cosVal );
            double t( TRANSLATE_PAN_RADIUS * sinVal );

            vertices->push_back( osg::Vec3( s, 0.0, t ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_FAN, 0, vertices->size() ) );

        SetDrawableToAlwaysCull( *geometry.get() );
        billboard->addDrawable( geometry.get() );
    }

    //Add rotation axis to the scene
    addChild( billboard.get() );
}
////////////////////////////////////////////////////////////////////////////////
