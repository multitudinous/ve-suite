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
#include <ves/xplorer/scenegraph/manipulator/ScaleUniform.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>
#include <osg/LineStipple>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ScaleUniform::ScaleUniform()
    :
    Dragger( TransformationType::SCALE_UNIFORM )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
ScaleUniform::ScaleUniform(
    const ScaleUniform& scaleUniform, const osg::CopyOp& copyop )
    :
    Dragger( scaleUniform, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ScaleUniform::~ScaleUniform()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ScaleUniform::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) )
    {
        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
}
////////////////////////////////////////////////////////////////////////////////
const char* ScaleUniform::className() const
{
    return "ScaleUniform";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ScaleUniform::clone( const osg::CopyOp& copyop ) const
{
    return new ScaleUniform( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* ScaleUniform::cloneType() const
{
    return new ScaleUniform();
}
////////////////////////////////////////////////////////////////////////////////
bool ScaleUniform::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const ScaleUniform* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* ScaleUniform::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void ScaleUniform::SetupDefaultGeometry()
{
    //The geode to add the geometry to
    osg::ref_ptr< osg::Geode > geode = new osg::Geode();

    //Create a box
    {
        osg::ref_ptr< osg::Box > box =
            new osg::Box( osg::Vec3d( 0.0, 0.0, 0.0 ), BOX_WIDTH );

        geode->addDrawable( new osg::ShapeDrawable( box.get() ) );
    }

    //Add box to the scene
    addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////
