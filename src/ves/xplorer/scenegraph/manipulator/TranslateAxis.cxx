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
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>
#include <osg/LineStipple>
#include <osg/PolygonStipple>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis()
    :
    Dragger()
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis(
    const TranslateAxis& translateAxis, const osg::CopyOp& copyop )
    :
    Dragger( translateAxis, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::~TranslateAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool TranslateAxis::Handle( Event::Enum event )
{
    //std::find( 
    //if( ( this ) )
    {
        UseColor( ColorTag::DEFAULT );

        return false;
    }

    switch( event )
    {
        case Event::FOCUS:
        {
            UseColor( ColorTag::FOCUS );

            return true;
        }
        case Event::PUSH:
        {
            UseColor( ColorTag::ACTIVE );

            return true;
        }
        case Event::DRAG:
        {
            return true;
        }
        case Event::RELEASE:
        {
            UseColor( ColorTag::DEFAULT );

            return true;
        }
        default:
        {
            return false;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::SetupDefaultGeometry()
{
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    double coneRadius = 0.05;
    double coneHeight = 0.2;
    osg::Vec3 coneCenter( coneHeight / 4.0, 0.0, 0.0 );

    //Create the positive axis
    {
        //Create a right line
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        vertices->resize( 2 );
        (*vertices)[ 0 ] = osg::Vec3( 0.0, 0.0, 0.0 );
        (*vertices)[ 1 ] = osg::Vec3( 1.0, 0.0, 0.0 );

        osg::Vec3 zAxis( 0.0, 0.0, 1.0 );
        osg::Quat rotation;
        rotation.makeRotate( zAxis, (*vertices)[ 1 ] );

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );
        geode->addDrawable( geometry.get() );
    
        //Create a right cone
        (*vertices)[ 1 ].x() -= coneHeight;
        osg::ref_ptr< osg::Cone > cone =
            new osg::Cone(
                (*vertices)[ 1 ] + coneCenter, coneRadius, coneHeight );
        cone->setRotation( rotation );
        geode->addDrawable( new osg::ShapeDrawable( cone.get() ) );

        //Create an invisible cylinder for picking the right line
        osg::ref_ptr< osg::Cylinder > cylinder =
            new osg::Cylinder(
                (*vertices)[ 1 ] * 0.5, coneRadius, (*vertices)[ 1 ].x() );
        cylinder->setRotation( rotation );
        osg::ref_ptr< osg::Drawable > drawable =
            new osg::ShapeDrawable( cylinder.get() );
        SetDrawableToAlwaysCull( *drawable.get() );
        geode->addDrawable( drawable.get() );
    }

    //Create the negative axis
    {
        //Create a left line
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        vertices->resize( 2 );
        (*vertices)[ 0 ] = osg::Vec3(  0.0, 0.0, 0.0 );
        (*vertices)[ 1 ] = osg::Vec3( -1.0, 0.0, 0.0 );

        osg::Vec3 zAxis( 0.0, 0.0, 1.0 );
        osg::Quat rotation;
        rotation.makeRotate( (*vertices)[ 1 ], zAxis );

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );
        geode->addDrawable( geometry.get() );

        //Create a left cone
        (*vertices)[ 1 ].x() += coneHeight;
        osg::ref_ptr< osg::Cone > cone =
            new osg::Cone(
                (*vertices)[ 1 ] - coneCenter, coneRadius, -coneHeight );
        cone->setRotation( rotation );
        geode->addDrawable( new osg::ShapeDrawable( cone.get() ) );

        //Create an invisible cylinder for picking the line
        osg::ref_ptr< osg::Cylinder > cylinder =
            new osg::Cylinder(
                (*vertices)[ 1 ] * 0.5, coneRadius, -(*vertices)[ 1 ].x() );
        cylinder->setRotation( rotation );
        osg::ref_ptr< osg::Drawable > drawable =
            new osg::ShapeDrawable( cylinder.get() );
        SetDrawableToAlwaysCull( *drawable.get() );
        geode->addDrawable( drawable.get() );
    }

    //Set line width
    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 1.0 );
    stateSet->setAttributeAndModes( lineWidth.get(), osg::StateAttribute::ON );

    //Set line stipple
    osg::ref_ptr< osg::LineStipple > lineStipple = new osg::LineStipple();
    lineStipple->setFactor( 1 );
    lineStipple->setPattern( 0xAAAA );
    stateSet->setAttributeAndModes( lineStipple.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    //Set polygon stipple
    osg::ref_ptr< osg::PolygonStipple > polygonStipple =
        new osg::PolygonStipple();
    GLubyte halftone[] =
    {
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55,
        0xAA, 0xAA, 0xAA, 0xAA, 0x55, 0x55, 0x55, 0x55
    };
    polygonStipple->setMask( halftone );
    stateSet->setAttributeAndModes( polygonStipple.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    stateSet->setMode( GL_LINE_SMOOTH,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    osg::ref_ptr< osg::Hint > hint =
        new osg::Hint( GL_LINE_SMOOTH_HINT, GL_NICEST );
    stateSet->setAttributeAndModes( hint.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    //Add lines and cones to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
