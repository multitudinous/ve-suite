/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/scenegraph/manipulator/RotateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/RotateAxis.h>
#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>
#include <ves/xplorer/scenegraph/manipulator/HelpCircle.h>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateCompound::RotateCompound()
    :
    CompoundDragger( TransformationType::ROTATE_COMPOUND ),
    m_xRotateAxis( NULL ),
    m_yRotateAxis( NULL ),
    m_zRotateAxis( NULL ),
    m_rotateTwist( NULL )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
RotateCompound::RotateCompound(
    const RotateCompound& rotateCompound, const osg::CopyOp& copyop )
    :
    CompoundDragger( rotateCompound, copyop ),
    m_xRotateAxis( rotateCompound.m_xRotateAxis.get() ),
    m_yRotateAxis( rotateCompound.m_yRotateAxis.get() ),
    m_zRotateAxis( rotateCompound.m_zRotateAxis.get() ),
    m_rotateTwist( rotateCompound.m_rotateTwist.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateCompound::~RotateCompound()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
RotateCompound* RotateCompound::AsRotateCompound()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
const char* RotateCompound::className() const
{
    return "RotateCompound";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateCompound::clone( const osg::CopyOp& copyop ) const
{
    return new RotateCompound( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* RotateCompound::cloneType() const
{
    return new RotateCompound();
}
////////////////////////////////////////////////////////////////////////////////
bool RotateCompound::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const RotateCompound* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
void RotateCompound::SetupDefaultGeometry()
{
    //Create rotate twist dragger
    m_rotateTwist = new RotateTwist();
    m_rotateTwist->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 1.0, 1.0, 1.0 ), true );

    addChild( m_rotateTwist.get() );

    //Create translate z-axis dragger
    m_zRotateAxis = new RotateAxis();
    m_zRotateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );

    addChild( m_zRotateAxis.get() );

    //Create translate y-axis dragger
    m_yRotateAxis = new RotateAxis();
    m_yRotateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );

    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 0.0, -1.0, 0.0 ) );
        m_yRotateAxis->SetRotation( rotation );
    }

    addChild( m_yRotateAxis.get() );

    //Create translate x-axis dragger
    m_xRotateAxis = new RotateAxis();
    m_xRotateAxis->SetColor(
        Color::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );

    //Rotate x-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate( GetUnitAxis(), osg::Vec3d( 1.0, 0.0, 0.0 ) );
        m_xRotateAxis->SetRotation( rotation );
    }

    addChild( m_xRotateAxis.get() );
}
////////////////////////////////////////////////////////////////////////////////
