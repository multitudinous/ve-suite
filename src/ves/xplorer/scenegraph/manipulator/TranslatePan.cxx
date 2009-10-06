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
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TranslatePan::TranslatePan()
    :
    Dragger( TransformationType::TRANSLATE_PAN )
{
    //If desktop mode
    //SetAutoRotateMode( AutoTransform::ROTATE_TO_SCREEN );
    //If cave mode
    SetAutoRotateMode( AutoTransform::ROTATE_TO_CAMERA );

    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    stateSet->setRenderBinDetails( 11, std::string( "RenderBin" ) );

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
TranslatePan* TranslatePan::AsTranslatePan()
{
    return this;
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
        m_rootDragger->GetPosition() + m_deltaTranslation;
    m_rootDragger->SetPosition( newTranslation );
}
////////////////////////////////////////////////////////////////////////////////
//See http://softsurfer.com/Archive/algorithm_0106/algorithm_0106.htm
const bool TranslatePan::ComputeProjectedPoint(
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
bool TranslatePan::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const TranslatePan* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void TranslatePan::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

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

            vertices->push_back( osg::Vec3( s, t, 0.0 ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_FAN, 0, vertices->size() ) );

        SetDrawableToAlwaysCull( *geometry.get() );
        geode->addDrawable( geometry.get() );
    }

    /*
    //Create invisible triangle strip for picking the rotation twist axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        double innerRadius( TRANSLATE_PAN_RADIUS - PICK_RADIUS );
        double outerRadius( TRANSLATE_PAN_RADIUS + PICK_RADIUS );
        for( unsigned int i = 0; i <= NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double si( innerRadius * cosVal );
            double ti( innerRadius * sinVal );

            double so( outerRadius * cosVal );
            double to( outerRadius * sinVal );

            vertices->push_back( osg::Vec3( si, ti, 0.0 ) );
            vertices->push_back( osg::Vec3( so, to, 0.0 ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_STRIP, 0, vertices->size() ) );

        SetDrawableToAlwaysCull( *geometry.get() );
        geode->addDrawable( geometry.get() );
    }
    */

    //Add rotation axis to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
