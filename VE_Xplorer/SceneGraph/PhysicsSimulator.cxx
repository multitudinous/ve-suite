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
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/CADEntity.h"
#include "VE_Xplorer/SceneGraph/DCS.h"

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/ReaderWriter>
#include <osgDB/Registry>
#include <osgDB/FileUtils>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>
#include <BulletCollision/CollisionDispatch/btSimulationIslandManager.h>

//#include "BulletCollision/CollisionDispatch/btSphereSphereCollisionAlgorithm.h"
//#include "../Extras/AlternativeCollisionAlgorithms/BoxBoxCollisionAlgorithm.h"
//#include "BulletCollision/CollisionDispatch/btSphereTriangleCollisionAlgorithm.h"

// --- C/C++ Libraries --- //
#include <sstream>
#include <ostream>
#include <string>

using namespace VE_SceneGraph;

const int maxProxies = 32766;

vprSingletonImp( PhysicsSimulator );

//#define PRINT_CONTACT_STATISTICS 1
//#define SHOW_NUM_DEEP_PENETRATIONS 1
//#define USE_KINEMATIC_GROUND 1
//#define USER_DEFINED_FRICTION_MODEL 1

#define USE_CUSTOM_NEAR_CALLBACK 1
#define USE_SWEEP_AND_PRUNE 1
//#define REGISTER_CUSTOM_COLLISION_ALGORITHM 1

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
:
idle( true ),
shoot_speed( 50.0f )
{
   head.init( "VJHead" );

   this->InitPhysics();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ExitPhysics()
{
   if( this->dynamics_world )
	{
	   //Remove the rigidbodies from the dynamics world and delete them
	   for(int i=0;i<dynamics_world->getNumCollisionObjects();i++)
		{
		   btCollisionObject* obj=dynamics_world->getCollisionObjectArray()[i];
		   dynamics_world->removeCollisionObject(obj);
		   delete obj;
	   }

      //Delete dynamics world
	   delete dynamics_world;
   }

   //*************************************************************************//
   //Don't know if btDynamicsWorld's destructor will clean these up in the future,
   //but it looks like they are still hanging around, so delete them for now.

   //Delete dispatcher
   if( this->dispatcher )
	{
	   delete this->dispatcher;
   }
   
   //Delete broadphase
   if( this->broadphase )
	{
	   delete this->broadphase;
   }

   //Delete solver
   if( this->solver )
	{
      delete this->solver;
   }
   //*************************************************************************//

	for( unsigned int i = 0; i < box_vector.size(); i++ )
	{
		delete box_vector.at( i );
	}

	box_vector.clear();
}
////////////////////////////////////////////////////////////////////////////////
//By default, Bullet will use its own nearcallback, but you can override it using dispatcher->setNearCallback()
void customNearCallback( btBroadphasePair& collisionPair, btCollisionDispatcher& dispatcher, btDispatcherInfo& dispatchInfo )
{
   btCollisionObject* colObj0 = (btCollisionObject*)collisionPair.m_pProxy0->m_clientObject;
	btCollisionObject* colObj1 = (btCollisionObject*)collisionPair.m_pProxy1->m_clientObject;

	if( dispatcher.needsCollision( colObj0, colObj1 ) )
   {
		//Dispatcher will keep algorithms persistent in the collision pair
		if( !collisionPair.m_algorithm )
      {
			collisionPair.m_algorithm = dispatcher.findAlgorithm( colObj0, colObj1 );
		}

		if( collisionPair.m_algorithm )
      {
			btManifoldResult contactPointResult( colObj0, colObj1 );
				
			if( dispatchInfo.m_dispatchFunc == btDispatcherInfo::DISPATCH_DISCRETE )
         {
				//Discrete collision detection query
				collisionPair.m_algorithm->processCollision( colObj0, colObj1, dispatchInfo, &contactPointResult );
			}
            
         else
         {
			   //Continuous collision detection query, time of impact (toi)
				float toi = collisionPair.m_algorithm->calculateTimeOfImpact( colObj0, colObj1, dispatchInfo, &contactPointResult );

            if( dispatchInfo.m_timeOfImpact > toi )
            {
					dispatchInfo.m_timeOfImpact = toi;
            }
         }
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::InitPhysics()
{
   dispatcher = new btCollisionDispatcher();

   #ifdef USE_CUSTOM_NEAR_CALLBACK
	   //This is optional
	   dispatcher->setNearCallback( customNearCallback );
   #endif //USE_CUSTOM_NEAR_CALLBACK

   #ifdef USE_SWEEP_AND_PRUNE
      btVector3 worldAabbMin( -10000, -10000, -10000 );
	   btVector3 worldAabbMax( 10000, 10000, 10000 );

	   broadphase = new btAxisSweep3( worldAabbMin, worldAabbMax, maxProxies );
   #else
      broadphase=new btSimpleBroadphase;
   #endif //USE_SWEEP_AND_PRUNE

   #ifdef REGISTER_CUSTOM_COLLISION_ALGORITHM
      //This is optional
   #else
      //Default constraint solver
      solver = new btSequentialImpulseConstraintSolver;
   #endif //REGISTER_CUSTOM_COLLISION_ALGORITHM
	
   dynamics_world = new btDiscreteDynamicsWorld( dispatcher, broadphase, solver );
	dynamics_world->setGravity( btVector3( 0, 0, -10 ) );

	//dynamics_world->setDebugDrawer(&debugDrawer);
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UpdatePhysics( float dt )
{
   if( !idle )
   {
	   dynamics_world->stepSimulation( dt );
	   /*
	   printf( "dt = %f: ", dt );

      if ( dynamics_world )
	   {
		   //during idle mode, just run 1 simulation step maximum
		   int maxSimSubSteps = idle ? 1 : 1;
		   if( idle )
		   {
			   dt = 1.0/420.f;
		   }

		   int numSimSteps = dynamics_world->stepSimulation( dt, maxSimSubSteps );
		   if( !numSimSteps )
		   {
				   printf( "Interpolated transforms\n" );
		   }

		   else
		   {
			   if( numSimSteps > maxSimSubSteps )
			   {
				   //detect dropping frames
				   printf( "Dropped (%i) simulation steps out of %i\n", numSimSteps - maxSimSubSteps, numSimSteps );
			   }

			   else
			   {
				   printf( "Simulated (%i) steps\n", numSimSteps );
			   }
		   }
	   }
	   */
   }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::StepSimulation()
{
   if( idle )
   {
      float dt = 1.0f / 60.f;

      dynamics_world->stepSimulation( dt );
   }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ResetScene()
{
   #ifdef SHOW_NUM_DEEP_PENETRATIONS
	   gNumDeepPenetrationChecks=0;
	   gNumGjkChecks=0;
   #endif //SHOW_NUM_DEEP_PENETRATIONS

	if(dynamics_world)
   {
		dynamics_world->stepSimulation(1.f/60.f,0);
	}

	int numObjects=dynamics_world->getNumCollisionObjects();
	
	for(int i=0;i<numObjects;i++)
   {
		btCollisionObject* colObj=dynamics_world->getCollisionObjectArray()[i];
		btRigidBody* body=btRigidBody::upcast(colObj);

		if(body && body->getMotionState())
      {
			btDefaultMotionState* myMotionState=(btDefaultMotionState*)body->getMotionState();
			myMotionState->m_graphicsWorldTrans=myMotionState->m_startWorldTrans;

			colObj->setWorldTransform(myMotionState->m_graphicsWorldTrans );
			colObj->setInterpolationWorldTransform(myMotionState->m_startWorldTrans);
			colObj->activate();

			//Removed cached contact points
			dynamics_world->getBroadphase()->cleanProxyFromPairs(colObj->getBroadphaseHandle());

			btRigidBody* body=btRigidBody::upcast(colObj);

			if(body&&!body->isStaticObject())
         {
				btRigidBody::upcast(colObj)->setLinearVelocity(btVector3(0,0,0));
				btRigidBody::upcast(colObj)->setAngularVelocity(btVector3(0,0,0));
			}
		}

	   /*
	   //quickly search some issue at a certain simulation frame, pressing space to reset
		int fixed=18;
		for (int i=0;i<fixed;i++)
		{
			getDynamicsWorld()->stepSimulation(1./60.f,1);
		}
      */
	}
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ShootBox( const btVector3& destination )
{
	if( dynamics_world )
	{
      //Create osg::Box to visually represent rigid body
      osg::ref_ptr< osg::Geode > geode = new osg::Geode;

		osg::ref_ptr< osg::Geometry > box = new osg::Geometry;

      osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array;

		//Left
	   vertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );

		//Near
	   vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
				
	   //Right
      vertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, 0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, 0.5f, 0.5f ) );

	   //Far
	   vertices->push_back( osg::Vec3( 0.5f, 0.5f, 0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, 0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );
				
	   //Top
	   vertices->push_back( osg::Vec3( -0.5f, 0.5f, 0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, -0.5f, 0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, -0.5f, 0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, 0.5f, 0.5f ) );
				
	   //Bottom
	   vertices->push_back( osg::Vec3( -0.5f, -0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( -0.5f, 0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, 0.5f, -0.5f ) );
      vertices->push_back( osg::Vec3( 0.5f, -0.5f, -0.5f ) );

		box->setVertexArray( vertices.get() );

		osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array;
		colors->push_back( osg::Vec4( 0.0, 1.0, 0.0, 1.0 ) );
		box->setColorArray( colors.get() );
		box->setColorBinding( osg::Geometry::BIND_OVERALL );

		osg::ref_ptr< osg::Vec3Array > normals = new osg::Vec3Array;
		normals->push_back( osg::Vec3( -1.0f, 0.0f, 0.0f ) );					//Left
		normals->push_back( osg::Vec3( 0.0f, -1.0f, 0.0f ) );					//Near
		normals->push_back( osg::Vec3( 1.0f, 0.0f, 0.0f ) );					//Right
		normals->push_back( osg::Vec3( 0.0f, 1.0f, 0.0f ) );					//Far
		normals->push_back( osg::Vec3( 0.0f, 0.0f, 1.0f ) );					//Top
		normals->push_back( osg::Vec3( 0.0f, 0.0f, -1.0f ) );					//Bottom
		box->setNormalArray( normals.get() );
		box->setNormalBinding( osg::Geometry::BIND_PER_PRIMITIVE );

		box->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::QUADS, 0, vertices.get()->size() ) );

		geode->addDrawable( box.get() );
		
		std::ostringstream box_ss;

		osgDB::writeNodeFile( *geode, "C:/Users/JK/Desktop/Models/box.osg" );

		osgDB::Registry::instance()->getReaderWriterForExtension("osg")->writeNode( *geode, box_ss );

		box_vector.push_back( new VE_SceneGraph::CADEntity( "C:/Users/JK/Desktop/Models/box.osg", VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS(), false ) );
		//box_vector.back()->SetPhysics( true );

		float mass = 1.0f;
		btTransform transform;
		transform.setIdentity();
      gadget::PositionData* head_pos;
      head_pos = head->getPositionData();
		btVector3 position;
      position.setValue( head_pos->mPosData[0][3], head_pos->mPosData[1][3], head_pos->mPosData[2][3] );
		transform.setOrigin( position );

		btCollisionShape* box_shape = new btBoxShape( btVector3( 1.0f, 1.0f, 1.0f ) );
		btRigidBody* body = this->CreateRigidBody( mass, transform, box_shape );

		btVector3 lin_vel( destination[0]+position[0], destination[1]+position[1], destination[2]+position[2] );
		lin_vel.normalize();
		lin_vel *= shoot_speed;

		body->getWorldTransform().setOrigin( position );
		body->getWorldTransform().setRotation( btQuaternion( 0, 0, 0, 1 ) );
		body->setLinearVelocity( lin_vel );
		body->setAngularVelocity( btVector3( 0, 0, 0 ) );
	}
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetIdle( bool state )
{
   idle = state;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetShootSpeed( float speed )
{
   shoot_speed = speed;
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* PhysicsSimulator::CreateRigidBody( float mass, const btTransform& startTransform, btCollisionShape* shape )
{
	//RigidBody is dynamic if and only if mass is non zero, otherwise static
	bool dynamic = (mass != 0.0f);

	btVector3 localInertia( 0 ,0, 0 );
   if( dynamic )
	{
		shape->calculateLocalInertia( mass, localInertia );
   }

	//Using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
	btDefaultMotionState* myMotionState = new btDefaultMotionState( startTransform );
	btRigidBody* body = new btRigidBody( mass, myMotionState, shape, localInertia );

	dynamics_world->addRigidBody( body );
	
	return body;
}
////////////////////////////////////////////////////////////////////////////////
btDynamicsWorld* PhysicsSimulator::GetDynamicsWorld()
{
   return dynamics_world;
}
////////////////////////////////////////////////////////////////////////////////
