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
 * Date modified: $Date: 2009-05-26 17:48:25 -0600 (Tue, 26 May 2009) $
 * Version:       $Rev: 12728 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: ClippingCircle.cxx 12728 2009-05-26 23:48:25Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/ClippingCircle.h>

// --- OSG Includes --- //
#include <osg/ColorMask>
#include <osg/Depth>
#include <osg/Stencil>
#include <osg/Hint>
#include <osg/Geometry>
#include <osg/LineWidth>

#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ClippingCircle::ClippingCircle()
    :
    osg::Billboard()
{
    setMode( osg::Billboard::POINT_ROT_EYE );

    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
ClippingCircle::ClippingCircle(
    const ClippingCircle& clippingCircle, const osg::CopyOp& copyop )
    :
    osg::Billboard( clippingCircle, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ClippingCircle::~ClippingCircle()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ClippingCircle::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) ) 
    {
        if( dynamic_cast< osgUtil::IntersectionVisitor* >( &nv ) )
        {
            return;
        }

        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
}
////////////////////////////////////////////////////////////////////////////////
const char* ClippingCircle::className() const
{
    return "ClippingCircle";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ClippingCircle::clone( const osg::CopyOp& copyop ) const
{
    return new ClippingCircle( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ClippingCircle::cloneType() const
{
    return new ClippingCircle();
}
////////////////////////////////////////////////////////////////////////////////
bool ClippingCircle::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const ClippingCircle* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* ClippingCircle::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void ClippingCircle::SetupDefaultGeometry()
{
    size_t numSegments( 100 );
    double radius( 1.0 );
    double TWO_PI( 2.0 * osg::PI );
    double ringDelta( TWO_PI / numSegments );

    //Create the clipping circle with line loops
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        for( size_t i = 0; i < numSegments; ++i )
        {
            double rot( i * ringDelta );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double x( radius * cosVal );
            double z( radius * sinVal );

            vertices->push_back( osg::Vec3( x, 0.0, z ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::LINE_LOOP, 0, vertices->size() ) );

        addDrawable( geometry.get() );

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

    //Create a triangle fan to act as the invisible clipping circle
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        vertices->push_back( osg::Vec3( 0.0, 0.0, 0.0 ) );
        for( size_t i = 0; i <= numSegments; ++i )
        {
            double rot( i * ringDelta );
            double cosVal( cos( rot ) );
            double sinVal( sin( rot ) );

            double x( radius * cosVal );
            double z( radius * sinVal );

            vertices->push_back( osg::Vec3( x, 0.0, z ) );
        }

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays(
                osg::PrimitiveSet::TRIANGLE_FAN, 0, vertices->size() ) );

        //addDrawable( geometry.get() );
        
        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            geometry->getOrCreateStateSet();

        //Turn color writes off
        osg::ref_ptr< osg::ColorMask > colorMask =
            new osg::ColorMask( false, false, false, false );
        stateSet->setAttributeAndModes( colorMask.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }
}
////////////////////////////////////////////////////////////////////////////////
