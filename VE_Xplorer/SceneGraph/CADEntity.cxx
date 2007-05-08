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
#endif

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>

// --- C/C++ Libraries --- //
#include <cassert>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( std::string geomFile, VE_SceneGraph::DCS* worldDCS, bool isStream )
{
   //Need to fix this and move some code to Node
   //Leave some code here no more FILEInfo
   this->dcs = new VE_SceneGraph::DCS();
   this->cad_helper = new VE_SceneGraph::CADEntityHelper();

   cad_helper->LoadFile( geomFile.c_str(), isStream );
   fileName.assign( geomFile );
   dcs->SetName( "CADEntityDCS" );
   dcs->addChild( cad_helper->GetNode() );
   worldDCS->AddChild( dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::CADEntity( VE_SceneGraph::CADEntityHelper* nodeToCopy, VE_SceneGraph::DCS* worldDCS )
{
   //Need to fix this and move some code to Node
   //Leave some code here no more FILEInfo
   this->dcs = new VE_SceneGraph::DCS();
   this->cad_helper = new VE_SceneGraph::CADEntityHelper( *nodeToCopy );

   fileName = cad_helper->GetNode()->getName();
   dcs->SetName( "CADEntityDCS" );
   dcs->addChild( cad_helper->GetNode() );
   worldDCS->AddChild( dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::~CADEntity( void )
{
   delete cad_helper;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::InitPhysics( void )
{
   rigid_body = new VE_SceneGraph::PhysicsRigidBody( cad_helper->GetNode() );

   dcs->SetbtRigidBody( rigid_body.get() );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntityHelper* CADEntity::GetNode( void )
{
   return cad_helper;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* CADEntity::GetDCS( void )
{
   return dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::PhysicsRigidBody* CADEntity::GetRigidBody( void )
{
   return rigid_body.get();
}
////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetFilename( void )
{
   return fileName;
}
////////////////////////////////////////////////////////////////////////////////
bool CADEntity::GetTransparentFlag( void )
{
   return _transparencyFlag;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetTransparencyFlag( bool x )
{
   this->_transparencyFlag = x;
}
////////////////////////////////////////////////////////////////////////////////
