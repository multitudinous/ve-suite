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
#include <ves/xplorer/scenegraph/camera/CameraManager.h>
#include <ves/xplorer/scenegraph/camera/Camera.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //


using namespace ves::xplorer::scenegraph::camera;

////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager()
    :
    osg::Group(),
    m_enabled( false ),
    //NodeMask is an unsigned int
    m_nodeMask( 0xfffffffe )
{
    Enable();
}
////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager(
    const CameraManager& cameraManager, const osg::CopyOp& copyop )
    :
    osg::Group( cameraManager, copyop ),
    m_enabled( cameraManager.m_enabled ),
    m_nodeMask( cameraManager.m_nodeMask )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraManager::~CameraManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::addChild( Camera* child )
{
    return osg::Group::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
/*
osg::BoundingSphere CameraManager::computeBound() const
{
    osg::BoundingSphere bsphere;

    return bsphere;
}
*/
////////////////////////////////////////////////////////////////////////////////
void CameraManager::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        setNodeMask( m_nodeMask );
    }
    else
    {
        setNodeMask( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
Camera* CameraManager::ConvertNodeToCamera( osg::Node* node )
{
    return dynamic_cast< Camera* >( node );
}
////////////////////////////////////////////////////////////////////////////////
Camera* CameraManager::GetChild( unsigned int i )
{
    return dynamic_cast< Camera* >( osg::Group::getChild( i ) );
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::insertChild( unsigned int index, Camera* child )
{
    return osg::Group::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
const bool CameraManager::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::replaceChild( Camera* origChild, Camera* newChild )
{
    return osg::Group::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::setChild( unsigned int i, Camera* node )
{
    return osg::Group::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
