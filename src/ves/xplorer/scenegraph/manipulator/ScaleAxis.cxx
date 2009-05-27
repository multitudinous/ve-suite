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
    Dragger()
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis::ScaleAxis(
    const ScaleAxis& scaleAxis, const osg::CopyOp& copyop )
    :
    Dragger( scaleAxis, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis::~ScaleAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleAxis::SetupDefaultGeometry()
{
    double boxWidth = 0.1;
    osg::Vec3 boxCenter( boxWidth * 0.5, 0.0, 0.0 );

    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //The unit axis
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->resize( 2 );
    (*vertices)[ 0 ] = osg::Vec3( 0.0, 0.0, 0.0 );
    (*vertices)[ 1 ] = osg::Vec3( 1.0, 0.0, 0.0 );

    //Rotation for boxes
    osg::Quat rotation;
    rotation.makeRotate( osg::Vec3( 0.0, 0.0, 1.0 ), (*vertices)[ 1 ] );

    //Create a positive line
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        vertices->resize( 2 );
        (*vertices)[ 0 ] = osg::Vec3( 0.0, 0.0, 0.0 );
        (*vertices)[ 1 ] = osg::Vec3( 1.0, 0.0, 0.0 );

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        geode->addDrawable( geometry.get() );

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

        //Set line stipple
        osg::ref_ptr< osg::LineStipple > lineStipple = new osg::LineStipple();
        lineStipple->setFactor( 2 );
        lineStipple->setPattern( 0xAAAA );
        stateSet->setAttributeAndModes( lineStipple.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create a positive box
    {
        (*vertices)[ 1 ].x() -= boxWidth;
        osg::ref_ptr< osg::Box > box =
            new osg::Box( (*vertices)[ 1 ] + boxCenter, boxWidth );
        box->setRotation( rotation );

        geode->addDrawable( new osg::ShapeDrawable( box.get() ) );
    }

    //Add lines and cones to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
