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
//#include <osg/NodeVisitor>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/MatrixTransform>
#include <osg/TriangleIndexFunctor>

//C/C++ libraries
#include <cassert>

using namespace VE_SceneGraph;

class TriIndexFunc
{
public:
   TriIndexFunc(){;}
   ~TriIndexFunc(){;}

   void inline operator()( unsigned int pos1, unsigned int pos2, unsigned int pos3 )
   {
      triangleIndex.push_back( pos1 );
      triangleIndex.push_back( pos2 );
      triangleIndex.push_back( pos3 );
   }

   std::vector< unsigned int > triangleIndex;
};
#endif

class Mesh : public osg::NodeVisitor
{
public:
	Mesh( osg::Node* osg_node )
	:
	NodeVisitor( TRAVERSE_ALL_CHILDREN )
	{
		triMesh = new btTriangleMesh;

		osg_node->accept( *this );
		this->CreateBBMesh();
		this->CreateExactMesh();
	}

	~Mesh()
	{
		if( collision_shape_bb )
		{
			delete collision_shape_bb;
		}

		if (collision_shape_exact )
		{
			delete collision_shape_exact;
		}
	}

	virtual void apply( osg::Geode& geode )
	{ 
		bb.expandBy( geode.getBoundingBox() );

		osg::TriangleIndexFunctor< TriIndexFunc > TIF;
		osg::ref_ptr< osg::Vec3Array > vertex_array;

		for( unsigned int i = 0; i < geode.getNumDrawables(); i++ )
		{
			geode.getDrawable( i )->accept( TIF );
			vertex_array = dynamic_cast< osg::Vec3Array* >( geode.getDrawable( i )->asGeometry()->getVertexArray() );

			btVector3 v1, v2, v3;

			for( unsigned int i = 0; i < TIF.triangleIndex.size()/3; i++ )
			{
				triMesh->addTriangle( btVector3( vertex_array->at( TIF.triangleIndex.at( i*3  ) ).x(),
															vertex_array->at( TIF.triangleIndex.at( i*3  ) ).y(),
															vertex_array->at( TIF.triangleIndex.at( i*3  ) ).z() ),
											 btVector3( vertex_array->at( TIF.triangleIndex.at( i*3+1) ).x(),
															vertex_array->at( TIF.triangleIndex.at( i*3+1) ).y(),
															vertex_array->at( TIF.triangleIndex.at( i*3+1) ).z() ),
											 btVector3( vertex_array->at( TIF.triangleIndex.at( i*3+2) ).x(),
															vertex_array->at( TIF.triangleIndex.at( i*3+2) ).y(),
															vertex_array->at( TIF.triangleIndex.at( i*3+2) ).z() ) );
			}

		}

	}

	void CreateBBMesh()
	{
		
		collision_shape_bb = new btBoxShape( btVector3( (bb.xMax()-bb.xMin())*0.5f,
																		(bb.yMax()-bb.yMin())*0.5f,
																		(bb.zMax()-bb.zMin())*0.5f ) );
	}

	void CreateExactMesh()
	{

		collision_shape_exact = new btBvhTriangleMeshShape( triMesh );
	}

	btCollisionShape* GetBBMesh()
	{
		return collision_shape_bb;
	}

	btCollisionShape* GetExactMesh()
	{
		return collision_shape_exact;
	}

private:
	osg::BoundingBox bb;
	btTriangleMesh* triMesh;

	btCollisionShape* collision_shape_bb;
	btCollisionShape* collision_shape_exact;
};

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

   #ifdef _PERFORMER
   fog = new pfFog();
   #elif _OSG
   //setup fog
   fog=new osg::Fog();
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
void CADEntity::InitPhysics()
{
	osg::ref_ptr< Mesh > mesh = new Mesh( this->node->GetNode() );

	collision_shape = mesh->GetBBMesh();

	btTransform transform;
	transform.setIdentity();
	this->rigid_body = VE_SceneGraph::PhysicsSimulator::instance()->CreateRigidBody( mass, transform, collision_shape );
	this->rigid_body->setFriction( this->friction );
	this->rigid_body->setRestitution( this->restitution );

	dcs->SetbtRigidBody( rigid_body );
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetMass( float m )
{
	mass = m;

	if( collision_shape )
	{
		//btRigidBody* is dynamic if and only if mass is non zero, otherwise static
		bool dynamic = (mass != 0.0f);

		btVector3 localInertia( 0, 0, 0 );
		if(dynamic)
		{
			collision_shape->calculateLocalInertia( mass, localInertia );
		}
	}
}
////////////////////////////////////////////////////////////////////////////////
void CADEntity::SetFriction( float f )
{
	friction=f;

	if(rigid_body){
		rigid_body->setFriction(friction);
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
/*
void CADEntity::UpdateMatTransform()
{
   osg::Matrix matrix;
   osg::Quat quat;

   btVector3 position;
   btQuaternion quaternion;

   if(rigid_body && rigid_body->getMotionState()){
      position=rigid_body->getWorldTransform().getOrigin();
      
      matrix.setTrans(position.getX(),position.getY(),position.getZ());
   
      btQuaternion quaternion=solid->getWorldTransform().getRotation();
      quat.set(quaternion.getX(),quaternion.getY(),quaternion.getZ(),quaternion.getW());
      matrix.setRotate(quat);
	}

   matTrans->setMatrix(matrix);
}
*/
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

