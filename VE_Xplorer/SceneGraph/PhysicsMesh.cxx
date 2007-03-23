#include "VE_Xplorer/SceneGraph/PhysicsMesh.h"

#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TriangleIndexFunctor>

#include <btBulletDynamicsCommon.h>

//C/C++ Libraries
#include <iostream>

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
	tri_mesh = 0;
   collision_shape = 0;

	osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
PhysicsMesh::~PhysicsMesh()
{
   if( tri_mesh )
	{
		delete tri_mesh;
	}

	if( collision_shape )
	{
		delete collision_shape;
	}
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsMesh::apply( osg::Geode& geode )
{ 
   tri_mesh = new btTriangleMesh;

	for( unsigned int i = 0; i < geode.getNumDrawables(); i++ )
	{
		osg::TriangleIndexFunctor< TriIndexFunc > TIF;
		osg::ref_ptr< osg::Vec3Array > vertex_array;

		geode.getDrawable( i )->accept( TIF );
		vertex_array = dynamic_cast< osg::Vec3Array* >( geode.getDrawable( i )->asGeometry()->getVertexArray() );

      for( unsigned int i = 0; i < geode.getNumDrawables(); i++ )
      {
		   bb.expandBy( geode.getDrawable( i )->getBound() );
      }

		btVector3 v1, v2, v3;

		for( unsigned int k = 0; k < TIF.triangleIndex.size()/3; k++ )
		{
			tri_mesh->addTriangle( btVector3( vertex_array->at( TIF.triangleIndex.at( k*3  ) ).x(),
														vertex_array->at( TIF.triangleIndex.at( k*3  ) ).y(),
														vertex_array->at( TIF.triangleIndex.at( k*3  ) ).z() ),
										 btVector3( vertex_array->at( TIF.triangleIndex.at( k*3+1) ).x(),
														vertex_array->at( TIF.triangleIndex.at( k*3+1) ).y(),
														vertex_array->at( TIF.triangleIndex.at( k*3+1) ).z() ),
										 btVector3( vertex_array->at( TIF.triangleIndex.at( k*3+2) ).x(),
														vertex_array->at( TIF.triangleIndex.at( k*3+2) ).y(),
														vertex_array->at( TIF.triangleIndex.at( k*3+2) ).z() ) );
		}
	}
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsMesh::CreateBoundingBoxShape()
{
   if( collision_shape )
   {
      delete collision_shape;
   }

	collision_shape = new btBoxShape( btVector3( ( bb.xMax() - bb.xMin() ) * 0.5f,
															   ( bb.yMax() - bb.yMin() ) * 0.5f,
																( bb.zMax() - bb.zMin() ) * 0.5f ) );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsMesh::CreateStaticConcaveShape()
{
   if( collision_shape )
   {
      delete collision_shape;
   }

	collision_shape = new btBvhTriangleMeshShape( tri_mesh, false );
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsMesh::CreateConvexShape()
{
   if( collision_shape )
   {
      delete collision_shape;
   }

	collision_shape = new btConvexTriangleMeshShape( tri_mesh );
}
////////////////////////////////////////////////////////////////////////////////
btCollisionShape* PhysicsMesh::GetCollisionShape()
{
   if( !collision_shape )
   {
      std::cout << "A collision shape has not yet been specified" << std::endl;
   }

   return collision_shape;
}
////////////////////////////////////////////////////////////////////////////////
