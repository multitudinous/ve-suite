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

using namespace ves::xplorer::scenegraph::manipulator;

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
void TranslateAxis::SetupDefaultGeometry()
{
    double cylinderRadius = 0.025;
    double coneRadius = 0.05;
    double coneHeight = 0.2;
    osg::Vec3 coneCenter( coneHeight * 0.25, 0.0, 0.0 );

    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //The unit axis
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->resize( 2 );
    (*vertices)[ 0 ] = osg::Vec3( 0.0, 0.0, 0.0 );
    (*vertices)[ 1 ] = osg::Vec3( 1.0, 0.0, 0.0 );

    //Rotation for cones and cylinders
    osg::Quat rotation;
    rotation.makeRotate( osg::Vec3( 0.0, 0.0, 1.0 ), (*vertices)[ 1 ] );

    //Create a positive line
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();

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
    }

    //Create a positive cone
    {
        (*vertices)[ 1 ].x() -= coneHeight;
        osg::ref_ptr< osg::Cone > cone =
            new osg::Cone(
                (*vertices)[ 1 ] + coneCenter, coneRadius, coneHeight );
        cone->setRotation( rotation );
        osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
            new osg::ShapeDrawable( cone.get() );

        geode->addDrawable( shapeDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            shapeDrawable->getOrCreateStateSet();

        //Set line hints
        stateSet->setMode( GL_POLYGON_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create an invisible cylinder for picking the positive line
    {
        osg::ref_ptr< osg::Cylinder > cylinder =
            new osg::Cylinder(
                (*vertices)[ 1 ] * 0.5, cylinderRadius, (*vertices)[ 1 ].x() );
        cylinder->setRotation( rotation );
        osg::ref_ptr< osg::ShapeDrawable > shapeDrawable =
            new osg::ShapeDrawable( cylinder.get() );
        SetDrawableToAlwaysCull( *shapeDrawable.get() );

        geode->addDrawable( shapeDrawable.get() );
    }

    //Add everything to this
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
