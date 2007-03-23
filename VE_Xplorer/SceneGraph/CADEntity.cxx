#include "VE_Xplorer/SceneGraph/CADEntity.h"

#include "VE_Xplorer/SceneGraph/CADEntityHelper.h"
#include "VE_Xplorer/SceneGraph/Geode.h"
#include "VE_Xplorer/SceneGraph/SceneNode.h"
#include "VE_Xplorer/SceneGraph/ModelOccluder.h"

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
mass(1.0f),
friction(0.5f),
restitution(0.0f)
{
   //Need to fix this and move some code to Node
   //Leave some code here no more FILEInfo
   this->dcs=new VE_SceneGraph::DCS();
   this->node=new VE_SceneGraph::CADEntityHelper();

   this->node->LoadFile( geomFile.c_str(),isStream );
   fileName.assign( geomFile );
	this->dcs->addChild( this->node->GetNode() );
   worldDCS->AddChild( this->dcs.get() );

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
void CADEntity::SetPhysics( bool p )
{
	if( p && !physics_mesh )
	{
		physics_mesh = new PhysicsMesh( this->node->GetNode() );
	}
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetCollisionShape( int collision_type )
{
	if( rigid_body )
	{
      VE_SceneGraph::PhysicsSimulator::instance()->GetDynamicsWorld()->removeRigidBody( rigid_body );
      
		delete rigid_body;
	}

   if( collision_type == 0 )
   {
      this->physics_mesh->CreateBoundingBoxShape();
   }

   else if( collision_type == 1 )
   {
      this->physics_mesh->CreateStaticConcaveShape();
   }

   else if( collision_type == 2 )
   {
      this->physics_mesh->CreateConvexShape();
   }

   collision_shape = this->physics_mesh->GetCollisionShape();

	btTransform transform;
		
	this->rigid_body = VE_SceneGraph::PhysicsSimulator::instance()->CreateRigidBody( mass, transform, collision_shape );
	this->rigid_body->setFriction( this->friction );
	this->rigid_body->setRestitution( this->restitution );

	dcs->SetbtRigidBody( rigid_body );
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::CADEntityHelper* CADEntity::GetNode()
{
   return this->node;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* CADEntity::GetDCS()
{
   return this->dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* CADEntity::GetRigidBody()
{
   return this->rigid_body;
}

////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetFilename()
{
   return this->fileName;
}
////////////////////////////////////////////////////////////////////////////////
std::string CADEntity::GetModuleName()
{
   return this->_moduleName;
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

