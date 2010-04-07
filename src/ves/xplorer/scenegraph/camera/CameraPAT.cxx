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
#include <ves/xplorer/scenegraph/camera/CameraPAT.h>
#include <ves/xplorer/scenegraph/camera/Camera.h>

using namespace ves::xplorer::scenegraph::camera;

////////////////////////////////////////////////////////////////////////////////
CameraPAT::CameraPAT( Camera& camera )
    :
    osg::PositionAttitudeTransform(),
    m_camera( camera )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraPAT::CameraPAT(
    const CameraPAT& cameraPAT,
    const osg::CopyOp& copyop )
    :
    osg::PositionAttitudeTransform( cameraPAT, copyop ),
    m_camera( cameraPAT.m_camera )
{
    if( &cameraPAT != this )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraPAT::~CameraPAT()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPAT::accept( osg::NodeVisitor& nv )
{
    PositionAttitudeTransform::accept( nv );
}
////////////////////////////////////////////////////////////////////////////////
bool CameraPAT::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const CameraPAT* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* CameraPAT::className() const
{
    return "CameraPAT";
}
////////////////////////////////////////////////////////////////////////////////
const char* CameraPAT::libraryName() const
{
    return "ves::xplorer::scenegraph::camera";
}
////////////////////////////////////////////////////////////////////////////////
void CameraPAT::setPosition( const osg::Vec3d& pos )
{
    _position = pos;
    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPAT::setAttitude( const osg::Quat& quat )
{
    _attitude = quat;
    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPAT::setScale( const osg::Vec3d& scale )
{
    _scale = scale;
    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
