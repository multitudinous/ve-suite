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
:
mass( 1.0f ),
friction( 1.0f ),
restitution( 0.0f ),
physics( false ),
concave( false )
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

	rigid_body = 0;
	physics_mesh = 0;
   collision_shape = 0;

   #ifdef _PERFORMER
   fog = new pfFog();
   #elif _OSG
   //setup fog
   fog = new osg::Fog();
   #endif
}
////////////////////////////////////////////////////////////////////////////////
CADEntity::~CADEntity()
{
	delete node;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::Initialize( float op_val )
{
   this->op = op_val;
   setOpac( op_val );
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetMass( float m )
{
   if( concave )
   {
      std::cout << "Static mesh cannot have an associated mass!" << std::endl;

      return;
   }

   else
   {
	   mass = m;

	   if( collision_shape )
	   {
		   //btRigidBody* is dynamic if and only if mass is non zero, otherwise static
		   bool dynamic = ( mass != 0.0f );

		   btVector3 localInertia( 0, 0, 0 );
		   if( dynamic )
		   {
			   collision_shape->calculateLocalInertia( mass, localInertia );
		   }
	   }
   }
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetFriction( float f )
{
	friction = f;

	if( rigid_body )
	{
		rigid_body->setFriction( friction );
	}
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetRestitution( float r )
{
	restitution = r;

	if( rigid_body )
	{
		rigid_body->setRestitution( restitution );
	}
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetCollisionShape( std::string type )
{
   if( !physics_mesh )
	{
		physics_mesh = new PhysicsMesh( node->GetNode() );
	}

   if( rigid_body )
   {
      VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( rigid_body );
      
      delete rigid_body;
   }

   if( type == "BoundingBox" )
   {
      concave = false;

      physics_mesh->CreateBoundingBoxShape();
   }

   else if( type == "StaticConcave" )
   {
      concave = true;

      if( mass != 0 )
      {
         mass = 0;
      }

      physics_mesh->CreateStaticConcaveShape();
   }

   else if( type == "Convex" )
   {
      concave = false;

      physics_mesh->CreateConvexShape();
   }

   collision_shape = physics_mesh->GetCollisionShape();

   btTransform transform;
		
   rigid_body = VE_SceneGraph::PhysicsSimulator::instance()->CreateRigidBody( mass, transform, collision_shape );
   rigid_body->setFriction( friction );
   rigid_body->setRestitution( restitution );

	dcs->SetbtRigidBody( rigid_body );
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
void CADEntity::SetRigidBody( btRigidBody* btRB )
{
   rigid_body = btRB;
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* CADEntity::GetRigidBody()
{
   return rigid_body;
}

////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetFilename()
{
   return fileName;
}
////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetModuleName()
{
   return _moduleName;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::GetColorArray()
{
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[0]  << " : " 
                      <<  this->_rgba[1]  <<  " : " << this->_rgba[2]  
                      << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
int CADEntity::GetTransparentFlag()
{
   return transparent;
}
////////////////////////////////////////////////////////////////////////////////
int CADEntity::GetColorFlag()
{
   return this->_colorFlag;
}
////////////////////////////////////////////////////////////////////////////////
float CADEntity::getOpacity()
{
   return this->op;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetFILEProperties( int color, int trans, float* stlColor )
{
   this->color = color;
   this->_colorFlag = color;
   this->transparent = trans;
   this->stlColor[0] = stlColor[0];
   this->stlColor[1] = stlColor[1];
   this->stlColor[2] = stlColor[2];
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::setOpac(float op_val)
{
   this->op = op_val;
   //this->node->SetNodeProperties( _colorFlag, op, stlColor );

   #ifdef _PERFORMER
      this->node->pfTravNodeMaterial( this->node->GetRawNode() );
   #elif _OSG
      //node->TravNodeMaterial(node->GetRawNode());
   #endif
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::setFog( double dist )
{
   #ifdef _PERFORMER
      fog->setColor( 0.6f, 0.6f, 0.6f);
      fog->setRange(0, dist);
      fog->setFogType(PFFOG_PIX_EXP2);
      this->node->pfTravNodeFog( this->node->GetRawNode(), fog );
   #elif _OSG
      fog->setMode( osg::Fog::EXP2 );
      fog->setDensity( 1 / ( dist / 2 ) );
      fog->setColor( osg::Vec4( 0.5f, 0.5f, 0.5f, 0.0f ) );
      //fog->setStart( 0.0f );
      //fog->setStart( dist + 100 );
      //fog->setEnd( dist + 200 );
      //fog->setFogCoordinateSource( );
      //this->node->TravNodeFog( this->node->GetRawNode(), fog );
   #endif
}
////////////////////////////////////////////////////////////////////////////////
/// Functions taken from module geometry for future merging
void CADEntity::SetRGBAColorArray( double* color )
{
   for( int i = 0; i < 4; i++ )
   {
      this->_rgba[i] = color[i];
   }
   vprDEBUG(vesDBG,2) << " Color ModuleGeometry: " << this->_rgba[ 0 ]  << " : " <<  this->_rgba[ 1 ]  <<  " : " << this->_rgba[ 2 ]  << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetTransparencyFlag( bool x )
{
   this->_transparencyFlag = x;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetOpacity( float x )
{
   this->_opacityLevel = x;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetColorFlag( int x )
{
   this->_colorFlag = x;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetModuleName( std::string filename )
{
   this->_moduleName = filename;
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::Update()
{
   std::cout << "Update Filename : " << this->_filename << std::endl
               << "trans : " << this->_transparencyFlag << std::endl
               << "op : " << this->_opacityLevel << std::endl
               << "color : " << this->_colorFlag << std::endl;
   // Fix this later to call traverser function
   //this->_node->SetColorOfGeometry( this->_node );
}
////////////////////////////////////////////////////////////////////////////////

