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
#include <ves/xplorer/scenegraph/CADEntity.h>

#include <ves/xplorer/scenegraph/CADEntityHelper.h>
#include <ves/xplorer/scenegraph/SceneNode.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Fog>
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#endif //_OSG

// --- C/C++ Libraries --- //
#include <cassert>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( std::string geomFile,
                      ves::xplorer::scenegraph::DCS* parentDCS,
                      bool isStream,
                      bool occlude,
                      PhysicsSimulator* physicsSimulator )
        :
        m_physicsRigidBody( 0 ),
        m_physicsFlag( false ),
        m_transparencyFlag( false ),
        m_physicsSimulator( physicsSimulator )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    m_dcs = new ves::xplorer::scenegraph::DCS();
    m_cadEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper();

    m_cadEntityHelper->LoadFile( geomFile.c_str(), isStream, occlude );
    m_fileName.assign( geomFile );
    m_dcs->SetName( "CADEntityDCS" );
    m_dcs->addChild( m_cadEntityHelper->GetNode() );
    /*if( occlude )
    {
        m_cadEntityHelper->AddOccluderNodes();
    }*/
    parentDCS->AddChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( osg::Node* node,
                      ves::xplorer::scenegraph::DCS* parentDCS,
                      PhysicsSimulator* physicsSimulator )
        :
        m_physicsRigidBody( 0 ),
        m_physicsFlag( false ),
        m_transparencyFlag( false ),
        m_physicsSimulator( physicsSimulator )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    m_dcs = new ves::xplorer::scenegraph::DCS();
    m_cadEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper();

    m_cadEntityHelper->SetNode( node );
    m_fileName.assign( "" );
    m_dcs->SetName( "CADEntityDCS" );
    m_dcs->addChild( m_cadEntityHelper->GetNode() );
    parentDCS->AddChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( ves::xplorer::scenegraph::CADEntityHelper* nodeToCopy,
                      ves::xplorer::scenegraph::DCS* parentDCS,
                      PhysicsSimulator* physicsSimulator )
        :
        m_physicsRigidBody( 0 ),
        m_physicsFlag( false ),
        m_transparencyFlag( false ),
        m_physicsSimulator( physicsSimulator )
{
    //Need to fix this and move some code to Node
    //Leave some code here no more FILEInfo
    m_dcs = new ves::xplorer::scenegraph::DCS();
    m_cadEntityHelper = new ves::xplorer::scenegraph::CADEntityHelper( *nodeToCopy );

    m_fileName = m_cadEntityHelper->GetNode()->getName();
    m_dcs->SetName( "CADEntityDCS" );
    m_dcs->addChild( m_cadEntityHelper->GetNode() );
    parentDCS->AddChild( m_dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::~CADEntity()
{
    delete m_cadEntityHelper;
    
    if( m_physicsRigidBody )
    {
        delete m_physicsRigidBody;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::InitPhysics()
{
    if( !m_physicsRigidBody )
    {
        m_physicsRigidBody = new ves::xplorer::scenegraph::PhysicsRigidBody(
                                 m_dcs.get(), m_physicsSimulator );
        m_dcs->SetPhysicsRigidBody( m_physicsRigidBody );
    }
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::CADEntityHelper* CADEntity::GetNode()
{
    return m_cadEntityHelper;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* CADEntity::GetDCS()
{
    return m_dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::PhysicsRigidBody* CADEntity::GetPhysicsRigidBody()
{
    return m_physicsRigidBody;
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
