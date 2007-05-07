/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"
#include "VE_Xplorer/SceneGraph/SceneNode.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#ifdef _PERFORMER
#include <Performer/pr/pfFog.h>
#elif _OSG
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"
#include <btBulletDynamicsCommon.h>

#include <osg/Fog>
#include <osg/Node>
#include <osg/Group>
#include <osg/MatrixTransform>
#endif

//C/C++ libraries
#include <cassert>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( std::string geomFile, VE_SceneGraph::DCS* worldDCS, bool isStream )
{
   //Need to fix this and move some code to Node
   //Leave some code here no more FILEInfo
   this->dcs = new VE_SceneGraph::DCS();
   dcs->SetName( "CADEntityDCS" );
   this->node = new VE_SceneGraph::CADEntityHelper();

   node->LoadFile( geomFile.c_str(), isStream );
   fileName.assign( geomFile );
	dcs->addChild( node->GetNode() );
   worldDCS->AddChild( dcs.get() );

   rigid_body = new VE_SceneGraph::Utilities::PhysicsRigidBody( node->GetNode() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( VE_SceneGraph::CADEntityHelper* nodeToCopy, VE_SceneGraph::DCS* worldDCS )
{
   //Need to fix this and move some code to Node
   //Leave some code here no more FILEInfo
   this->dcs = new VE_SceneGraph::DCS();
   dcs->SetName( "CADEntityDCS" );
   this->node = new VE_SceneGraph::CADEntityHelper( *nodeToCopy );
   fileName = node->GetNode()->getName();

	dcs->addChild( node->GetNode() );
   worldDCS->AddChild( dcs.get() );

   rigid_body = new VE_SceneGraph::Utilities::PhysicsRigidBody( node->GetNode() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::~CADEntity()
{
	delete node;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetMass( float mass )
{
   if( rigid_body.valid() )
   {
      rigid_body->setMass( mass );
   }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetFriction( float friction )
{
	if( rigid_body.valid() )
	{
		rigid_body->setFriction( friction );
	}
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetRestitution( float restitution )
{
	if( rigid_body.valid() )
	{
		rigid_body->setRestitution( restitution );
	}
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetRigidBody( std::string type )
{
   if( rigid_body.valid() )
   {
      VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( rigid_body.get() );
   }

   if( type == "BoundingBox" )
   {
      rigid_body->CreateBoundingBoxShape();
   }

   else if( type == "StaticConcave" )
   {
      rigid_body->setMass( 0.0f );

      rigid_body->CreateStaticConcaveShape();
   }

   else if( type == "Convex" )
   {
      rigid_body->CreateConvexShape();
   }

   btTransform transform;
   transform.setIdentity();

	//Using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
	btDefaultMotionState* motion_state = new btDefaultMotionState( transform );
   rigid_body->setMotionState( motion_state );
   
   VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->addRigidBody( rigid_body.get() );

	dcs->SetbtRigidBody( rigid_body.get() );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntityHelper* CADEntity::GetNode()
{
   return node;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* CADEntity::GetDCS()
{
   return dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* CADEntity::GetRigidBody()
{
   return rigid_body.get();
}

////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetFilename()
{
   return fileName;
}
////////////////////////////////////////////////////////////////////////////////
bool CADEntity::GetTransparentFlag()
{
   return _transparencyFlag;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetTransparencyFlag( bool x )
{
   this->_transparencyFlag = x;
}
