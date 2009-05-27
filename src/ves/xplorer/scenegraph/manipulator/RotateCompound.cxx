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
#include <ves/xplorer/scenegraph/manipulator/RotateCompound.h>
#include <ves/xplorer/scenegraph/manipulator/RotateAxis.h>
#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>

// --- OSG Includes --- //

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
RotateCompound::RotateCompound()
    :
    CompoundDragger(),
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
void RotateCompound::SetupDefaultGeometry()
{
    //Create translate x-axis dragger
    m_xRotateAxis = new RotateAxis();
    m_xRotateAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 1.0, 0.0, 0.0, 1.0 ), true );

    addChild( m_xRotateAxis.get() );

    //Create translate y-axis dragger
    m_yRotateAxis = new RotateAxis();
    m_yRotateAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 0.0, 1.0, 0.0, 1.0 ), true );

    //Rotate y-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 1.0, 0.0 ) );
        m_yRotateAxis->setMatrix( osg::Matrix( rotation ) );
    }

    addChild( m_yRotateAxis.get() );

    //Create translate z-axis dragger
    m_zRotateAxis = new RotateAxis();
    m_zRotateAxis->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 0.0, 0.0, 1.0, 1.0 ), true );

    //Rotate z-axis dragger appropriately
    {
        osg::Quat rotation;
        rotation.makeRotate(
            osg::Vec3d( 1.0, 0.0, 0.0 ), osg::Vec3d( 0.0, 0.0, 1.0 ) );
        m_zRotateAxis->setMatrix( osg::Matrix( rotation ) );
    }

    addChild( m_zRotateAxis.get() );

    //Create rotate twist dragger
    m_rotateTwist = new RotateTwist();
    m_rotateTwist->SetColor(
        ColorTag::DEFAULT, osg::Vec4f( 1.0, 1.0, 1.0, 1.0 ), true );

    addChild( m_rotateTwist.get() );
}
////////////////////////////////////////////////////////////////////////////////
