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
#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/Geode>
#include <osg/Geometry>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateTwist::RotateTwist()
    :
    Rotate( TransformationType::ROTATE_TWIST )
{
    if( ves::xplorer::scenegraph::SceneManager::instance()->IsDesktopMode() )
    {
        //If desktop mode
        SetAutoRotateMode( AutoTransform::ROTATE_TO_SCREEN );
    }
    else
    {
        //If cave mode
        SetAutoRotateMode( AutoTransform::NO_ROTATION );
    }

    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist::RotateTwist(
    const RotateTwist& rotateTwist, const osg::CopyOp& copyop )
    :
    Rotate( rotateTwist, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist::~RotateTwist()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist* RotateTwist::AsRotateTwist()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
const char* RotateTwist::className() const
{
    return "RotateTwist";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateTwist::clone( const osg::CopyOp& copyop ) const
{
    return new RotateTwist( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateTwist::cloneType() const
{
    return new RotateTwist();
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::CustomPushAction()
{
    Rotate::CustomPushAction();
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::CustomDragAction()
{
    Rotate::CustomDragAction();
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::CustomReleaseAction()
{
    Rotate::CustomReleaseAction();
}
////////////////////////////////////////////////////////////////////////////////
const double& RotateTwist::GetRadius() const
{
    return ROTATE_TWIST_RADIUS;
}
////////////////////////////////////////////////////////////////////////////////
bool RotateTwist::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const RotateTwist* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void RotateTwist::SetupDefaultGeometry()
{
    //
    Rotate::SetupDefaultGeometry();

    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //Create invisible triangle strip for picking the rotation twist axis
    {
        osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
        osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();

        double innerRadius( ROTATE_TWIST_RADIUS - PICK_RADIUS );
        double outerRadius( ROTATE_TWIST_RADIUS + PICK_RADIUS );
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

    //Add rotation axis to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
