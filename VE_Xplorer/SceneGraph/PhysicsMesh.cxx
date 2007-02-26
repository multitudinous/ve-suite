#include "VE_Xplorer/SceneGraph/PhysicsMesh.h"

#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TriangleIndexFunctor>

#include <btBulletDynamicsCommon.h>

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

////////////////////////////////////////////////////////////////////////////////
PhysicsMesh::PhysicsMesh( osg::Node* osg_node )
:
NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
	triMesh = new btTriangleMesh;

	osg_node->accept( *this );
	this->CreateBBMesh();
	this->CreateExactMesh();
}
////////////////////////////////////////////////////////////////////////////////
PhysicsMesh::~PhysicsMesh()
{
	if( collision_shape_bb )
	{
		delete collision_shape_bb;
	}

	if( collision_shape_exact )
	{
		delete collision_shape_exact;
	}
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsMesh::apply( osg::Geode& geode )
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
////////////////////////////////////////////////////////////////////////////////
void PhysicsMesh::CreateBBMesh()
{
	/*
	collision_shape_bb = new btBoxShape( btVector3( (bb.xMax()-bb.xMin())*0.5f,
																	(bb.yMax()-bb.yMin())*0.5f,
																	(bb.zMax()-bb.zMin())*0.5f ) );
																	*/

	collision_shape_bb = new btBoxShape( btVector3( 1, 1 ,1 ) );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsMesh::CreateExactMesh()
{
	//collision_shape_exact = new btBvhTriangleMeshShape( triMesh, false );
}
////////////////////////////////////////////////////////////////////////////////
btCollisionShape* PhysicsMesh::GetBBMesh()
{
	return collision_shape_bb;
}
////////////////////////////////////////////////////////////////////////////////
btCollisionShape* PhysicsMesh::GetExactMesh()
{
	return collision_shape_exact;
}
////////////////////////////////////////////////////////////////////////////////
