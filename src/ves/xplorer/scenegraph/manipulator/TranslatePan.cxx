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
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geometry>
#include <osg/Billboard>
#include <osg/LineWidth>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
TranslatePan::TranslatePan(
    Manipulator* const parentManipulator )
    :
    Dragger(
        AxesFlag::BILLBOARD,
        TransformationType::TRANSLATE_PAN,
        parentManipulator )
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
    return new TranslatePan( m_parentManipulator );
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
        osg::ref_ptr< osg::Vec3dArray > vertices = new osg::Vec3dArray();
        for( size_t i = 0; i < NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double x( TRANSLATE_PAN_RADIUS * cosVal );
            double z( TRANSLATE_PAN_RADIUS * sinVal );

            vertices->push_back( osg::Vec3d( x, 0.0, z ) );
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
        osg::ref_ptr< osg::Vec3dArray > vertices = new osg::Vec3dArray();

        vertices->push_back( osg::Vec3d( 0.0, 0.0, 0.0 ) );
        for( size_t i = 0; i <= NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double x( TRANSLATE_PAN_RADIUS * cosVal );
            double z( TRANSLATE_PAN_RADIUS * sinVal );

            vertices->push_back( osg::Vec3d( x, 0.0, z ) );
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
