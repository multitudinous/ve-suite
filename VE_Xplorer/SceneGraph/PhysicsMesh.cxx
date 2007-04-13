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
#include "VE_Xplorer/SceneGraph/PhysicsMesh.h"

#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TriangleIndexFunctor>

#include <btBulletDynamicsCommon.h>

// --- C/C++ Libraries --- //
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

	for( size_t i = 0; i < geode.getNumDrawables(); i++ )
	{
		osg::TriangleIndexFunctor< TriIndexFunc > TIF;
		osg::ref_ptr< osg::Vec3Array > vertex_array;

		geode.getDrawable( i )->accept( TIF );
		vertex_array = static_cast< osg::Vec3Array* >( geode.getDrawable( i )->asGeometry()->getVertexArray() );

		bb.expandBy( geode.getDrawable( i )->getBound() );

		btVector3 v1, v2, v3;

		for( size_t j = 0; j < TIF.triangleIndex.size() / 3; j++ )
		{
			tri_mesh->addTriangle( btVector3( vertex_array->at( TIF.triangleIndex.at( j*3  ) ).x(),
														 vertex_array->at( TIF.triangleIndex.at( j*3  ) ).y(),
														 vertex_array->at( TIF.triangleIndex.at( j*3  ) ).z() ),
										  btVector3( vertex_array->at( TIF.triangleIndex.at( j*3+1) ).x(),
														 vertex_array->at( TIF.triangleIndex.at( j*3+1) ).y(),
														 vertex_array->at( TIF.triangleIndex.at( j*3+1) ).z() ),
										  btVector3( vertex_array->at( TIF.triangleIndex.at( j*3+2) ).x(),
														 vertex_array->at( TIF.triangleIndex.at( j*3+2) ).y(),
														 vertex_array->at( TIF.triangleIndex.at( j*3+2) ).z() ) );
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
