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
#include <ves/xplorer/device/manipulator/Translate1D.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/MatrixTransform>
#include <osg/Material>
#include <osg/LineWidth>

using namespace ves::xplorer::device::manipulator;

////////////////////////////////////////////////////////////////////////////////
Translate1D::Translate1D()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Translate1D::~Translate1D()
{

}
////////////////////////////////////////////////////////////////////////////////
void Translate1D::SetupDefaultGeometry()
{
    //Get the line length and direction.
    //osg::Vec3 lineDir = _projector->getLineEnd()-_projector->getLineStart();
    //float lineLength = lineDir.length();
    //lineDir.normalize();

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //Create a line
    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    (*vertices)[ 0 ] = osg::Vec3d( 0.0, 0.0, 0.0 );
    (*vertices)[ 1 ] = osg::Vec3d( 1.0, 0.0, 0.0 );

    geometry->setVertexArray( vertices.get() );
    geometry->addPrimitiveSet(
        new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

    geode->addDrawable( geometry.get() );

    /*
    //Create a left cone
    {
        osg::ref_ptr< osg::Cone > cone =
            new osg::Cone( _projector->getLineStart(), 0.025f * lineLength, 0.10f * lineLength );
        osg::Quat rotation;
        rotation.makeRotate(lineDir, osg::Vec3(0.0f, 0.0f, 1.0f));
        cone->setRotation(rotation);

        geode->addDrawable( new osg::ShapeDrawable( cone ) );
    }
    */

    /*
    //Create a cone
    {
        osg::ref_ptr< osg::Cone > cone =
            new osg::Cone(_projector->getLineEnd(), 0.025f * lineLength, 0.10f * lineLength);
        osg::Quat rotation;
        rotation.makeRotate( osg::Vec3( 0.0, 0.0, 1.0 ), lineDir );
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

    //Add line and cones to the scene
    m_matrixTransform->addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
