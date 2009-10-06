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
#include <ves/xplorer/scenegraph/manipulator/HelpCircle.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/ClipNode>
#include <osg/ClipPlane>

#include <osgUtil/CullVisitor>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
HelpCircle::HelpCircle()
    :
    Dragger( TransformationType::HELP_CIRCLE ),
    m_clipNode( NULL )
{
    //If desktop mode
    //SetAutoRotateMode( AutoTransform::ROTATE_TO_SCREEN );
    //If cave mode
    SetAutoRotateMode( AutoTransform::ROTATE_TO_CAMERA );

    SetupDefaultGeometry();

    //Set up the clipping plane
    m_clipNode = new osg::ClipNode();
    m_clipNode->addClipPlane( new osg::ClipPlane( 0, GetUnitPlane() ) );
    addChild( m_clipNode.get() );
}
////////////////////////////////////////////////////////////////////////////////
HelpCircle::HelpCircle(
    const HelpCircle& rotateTwist, const osg::CopyOp& copyop )
    :
    Dragger( rotateTwist, copyop ),
    m_clipNode( rotateTwist.m_clipNode.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
HelpCircle::~HelpCircle()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
const char* HelpCircle::className() const
{
    return "HelpCircle";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* HelpCircle::clone( const osg::CopyOp& copyop ) const
{
    return new HelpCircle( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* HelpCircle::cloneType() const
{
    return new HelpCircle();
}
////////////////////////////////////////////////////////////////////////////////
const osg::ClipNode* const HelpCircle::GetClipNode() const
{
    return m_clipNode.get();
}
////////////////////////////////////////////////////////////////////////////////
bool HelpCircle::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const HelpCircle* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* HelpCircle::Release( osg::NodePath::iterator& npItr )
{
    //Remove Show() function from Dragger::Release

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
void HelpCircle::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //Create the clipping circle with line loops
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        for( unsigned int i = 0; i < NUM_CIRCLE_SEGMENTS; ++i )
        {
            double rot( i * DELTA_SEGMENT_ANGLE );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double s( HELP_CIRCLE_RADIUS * cosVal );
            double t( HELP_CIRCLE_RADIUS * sinVal );

            vertices->push_back( osg::Vec3d( s, t, 0.0 ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::LINE_LOOP, 0, vertices->size() ) );

        geode->addDrawable( geometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            geometry->getOrCreateStateSet();

        //Override color uniform
        stateSet->addUniform(
            new osg::Uniform( "color", GetColor( Color::DISABLED ) ) );

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( 2.0 );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );
    }

    //Add rotation axis to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
