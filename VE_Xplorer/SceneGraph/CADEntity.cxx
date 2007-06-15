/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Fog>
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#endif //_OSG

// --- C/C++ Libraries --- //
#include <cassert>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( std::string geomFile, 
    VE_SceneGraph::DCS* parentDCS, bool isStream, bool occlude )
:
m_physicsRigidBody( 0 ),
m_physicsFlag( false ),
m_transparencyFlag( false )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    m_dcs = new VE_SceneGraph::DCS();
    m_cadEntityHelper = new VE_SceneGraph::CADEntityHelper();

    m_cadEntityHelper->LoadFile( geomFile.c_str(), isStream, occlude );
    m_fileName.assign( geomFile );
    m_dcs->SetName( "CADEntityDCS" );
/*#ifdef VE_PATENTED
    osg::ref_ptr<osgOQ::OcclusionQueryRoot> osgOQRoot;
    // Some other plugin was used. Add it to an OQR.
    osgOQRoot = new osgOQ::OcclusionQueryRoot( 
        VE_SceneGraph::SceneManager::instance()->GetOcclusionQueryContext() );
    osgOQRoot->addChild( m_cadEntityHelper->GetNode() );
    m_dcs->addChild( osgOQRoot.get() );
#else*/
    m_dcs->addChild( m_cadEntityHelper->GetNode() );
//#endif
    parentDCS->AddChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( osg::Node* node, VE_SceneGraph::DCS* parentDCS )
:
m_physicsRigidBody( 0 ),
m_physicsFlag( false ),
m_transparencyFlag( false )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    m_dcs = new VE_SceneGraph::DCS();
    m_cadEntityHelper = new VE_SceneGraph::CADEntityHelper();

    m_cadEntityHelper->SetNode( node );
    m_fileName.assign( "" );
    m_dcs->SetName( "CADEntityDCS" );
    m_dcs->addChild( m_cadEntityHelper->GetNode() );
    parentDCS->AddChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( VE_SceneGraph::CADEntityHelper* nodeToCopy, 
    VE_SceneGraph::DCS* parentDCS )
:
m_physicsRigidBody( 0 ),
m_physicsFlag( false ),
m_transparencyFlag( false )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    m_dcs = new VE_SceneGraph::DCS();
    m_cadEntityHelper = new VE_SceneGraph::CADEntityHelper( *nodeToCopy );

    m_fileName = m_cadEntityHelper->GetNode()->getName();
    m_dcs->SetName( "CADEntityDCS" );
    m_dcs->addChild( m_cadEntityHelper->GetNode() );
    parentDCS->AddChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::~CADEntity()
{
    delete m_cadEntityHelper;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::InitPhysics()
{
    if( !m_physicsRigidBody.valid() )
    {
        m_physicsRigidBody = new VE_SceneGraph::PhysicsRigidBody( 
            m_cadEntityHelper->GetNode() );

        m_dcs->SetbtRigidBody( m_physicsRigidBody.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntityHelper* CADEntity::GetNode()
{
    return m_cadEntityHelper;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* CADEntity::GetDCS()
{
    return m_dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::PhysicsRigidBody* CADEntity::GetPhysicsRigidBody()
{
    return m_physicsRigidBody.get();
}
////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetFilename()
{
    return m_fileName;
}
////////////////////////////////////////////////////////////////////////////////
bool CADEntity::GetTransparentFlag()
{
    return m_transparencyFlag;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetTransparencyFlag( bool flag )
{
    m_transparencyFlag = flag;
}
////////////////////////////////////////////////////////////////////////////////
