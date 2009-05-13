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

}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::SetupDefaultGeometry()
{
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //Create a right line
    //{
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
        vertices->resize( 2 );
        (*vertices)[ 0 ] = osg::Vec3d( 0.0, 0.0, 0.0 );
        (*vertices)[ 1 ] = osg::Vec3d( 1.0, 0.0, 0.0 );

        geometry->setVertexArray( vertices.get() );
        geometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        geode->addDrawable( geometry.get() );
    //}

    //Create a right cone
    //{
        osg::ref_ptr< osg::Cone > cone =
            new osg::Cone( (*vertices)[ 1 ], 0.05, 0.2 );
        osg::Quat rotation;
        rotation.makeRotate( osg::Vec3( 0.0, 0.0, 1.0 ), (*vertices)[ 1 ] );
        cone->setRotation( rotation );

        geode->addDrawable( new osg::ShapeDrawable( cone.get() ) );
    //}

    /*
    //Create a left cone
    {
        osg::ref_ptr< osg::Cone > cone =
            new osg::Cone( _projector->getLineStart(), 0.025 * lineLength, 0.1 * lineLength );
        osg::Quat rotation;
        rotation.makeRotate(lineDir, osg::Vec3(0.0, 0.0f, 1.0));
        cone->setRotation(rotation);

        geode->addDrawable( new osg::ShapeDrawable( cone ) );
    }
    */



    /*
    //Create an invisible cylinder for picking the line
    {
        osg::ref_ptr< osg::Cylinder > cylinder =
            new osg::Cylinder( ( _projector->getLineStart() + _projector->getLineEnd() ) / 2, 0.015 * lineLength, lineLength );
        osg::Quat rotation;
        rotation.makeRotate(osg::Vec3(0.0f, 0.0f, 1.0f), lineDir);
        cylinder->setRotation(rotation);
        osg::Drawable* cylinderGeom = new osg::ShapeDrawable( cylinder );

        setDrawableToAlwaysCull( *cylinderGeom );
    
        geode->addDrawable( cylinderGeom );
    }
    */

    //Turn off lighting for line and set line width
    osg::ref_ptr< osg::StateSet > stateSet = geode->getOrCreateStateSet();
    osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
    lineWidth->setWidth( 2.0 );
    stateSet->setAttributeAndModes( lineWidth.get(), osg::StateAttribute::ON );
    stateSet->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    stateSet->setMode( GL_LINE_SMOOTH,
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    osg::ref_ptr< osg::Hint > hint =
        new osg::Hint( GL_LINE_SMOOTH_HINT, GL_NICEST );
    stateSet->setAttributeAndModes( hint.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    //Add line and cones to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
