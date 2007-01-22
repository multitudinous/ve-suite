//#define PRINT_CONTACT_STATISTICS 1
//#define SHOW_NUM_DEEP_PENETRATIONS 1
//#define USE_KINEMATIC_GROUND 1
//#define USER_DEFINED_FRICTION_MODEL 1

#define USE_CUSTOM_NEAR_CALLBACK 1
#define USE_SWEEP_AND_PRUNE 1
//#define REGISTER_CUSTOM_COLLISION_ALGORITHM 1
#define USE_MOTIONSTATE 1

#include "PhysicsSimulator.h"

//Temporary pound define
#ifdef VE_PHYSICS

#include "btBulletDynamicsCommon.h"

//#include "BulletCollision/CollisionDispatch/btSphereSphereCollisionAlgorithm.h"
//#include "../Extras/AlternativeCollisionAlgorithms/BoxBoxCollisionAlgorithm.h"
//#include "BulletCollision/CollisionDispatch/btSphereTriangleCollisionAlgorithm.h"

#include <osg/ShapeDrawable>

using namespace VE_SceneGraph;

const int maxProxies=32766;

vprSingletonImp(PhysicsSimulator);

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
:
physics(true),
shoot_speed(40.0f)
{
   //head.init("VJHead");

   this->InitPhysics();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ExitPhysics()
{
   if(this->dynamics_world){
	   //Remove the rigidbodies from the dynamics world and delete them
	   for(int i=0;i<dynamics_world->getNumCollisionObjects();i++){
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
   if(this->dispatcher){
	   delete this->dispatcher;
   }
   
   //Delete broadphase
   if(this->broadphase){
	   delete this->broadphase;
   }

   //Delete solver
   if(this->solver){
      delete this->solver;
   }

   //*************************************************************************//
}
////////////////////////////////////////////////////////////////////////////////
//By default, Bullet will use its own nearcallback, but you can override it using dispatcher->setNearCallback()
void customNearCallback(btBroadphasePair& collisionPair, btCollisionDispatcher& dispatcher, btDispatcherInfo& dispatchInfo)
{
   btCollisionObject* colObj0=(btCollisionObject*)collisionPair.m_pProxy0->m_clientObject;
	btCollisionObject* colObj1=(btCollisionObject*)collisionPair.m_pProxy1->m_clientObject;

	if(dispatcher.needsCollision(colObj0,colObj1)){
		//Dispatcher will keep algorithms persistent in the collision pair
		if(!collisionPair.m_algorithm){
			collisionPair.m_algorithm=dispatcher.findAlgorithm(colObj0,colObj1);
		}

		if(collisionPair.m_algorithm){
			btManifoldResult contactPointResult(colObj0,colObj1);
				
			if(dispatchInfo.m_dispatchFunc==btDispatcherInfo::DISPATCH_DISCRETE){
				//Discrete collision detection query
				collisionPair.m_algorithm->processCollision(colObj0,colObj1,dispatchInfo,&contactPointResult);
			}
            
         else{
			   //Continuous collision detection query, time of impact (toi)
				float toi=collisionPair.m_algorithm->calculateTimeOfImpact(colObj0,colObj1,dispatchInfo,&contactPointResult);

            if(dispatchInfo.m_timeOfImpact>toi){
					dispatchInfo.m_timeOfImpact=toi;
            }
         }
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::InitPhysics()
{
   dispatcher=new btCollisionDispatcher();

   #ifdef USE_CUSTOM_NEAR_CALLBACK
	   //This is optional
	   dispatcher->setNearCallback(customNearCallback);
   #endif //USE_CUSTOM_NEAR_CALLBACK

   #ifdef USE_SWEEP_AND_PRUNE
      btVector3 worldAabbMin(-10000,-10000,-10000);
	   btVector3 worldAabbMax(10000,10000,10000);

	   broadphase=new btAxisSweep3(worldAabbMin,worldAabbMax,maxProxies);
   #else
      broadphase=new btSimpleBroadphase;
   #endif //USE_SWEEP_AND_PRUNE
	
   #ifdef REGISTER_CUSTOM_COLLISION_ALGORITHM
      //This is optional
   #else
      //Default constraint solver
      solver=new btSequentialImpulseConstraintSolver;
   #endif //REGISTER_CUSTOM_COLLISION_ALGORITHM
	
   dynamics_world=new btDiscreteDynamicsWorld(dispatcher,broadphase,solver);
	dynamics_world->setGravity(btVector3(0,0,-10));

	//dynamics_world->setDebugDrawer(&debugDrawer);
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::UpdatePhysics(float dt)
{
   dynamics_world->stepSimulation(dt);
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::ResetScene()
{
   #ifdef SHOW_NUM_DEEP_PENETRATIONS
	   gNumDeepPenetrationChecks=0;
	   gNumGjkChecks=0;
   #endif //SHOW_NUM_DEEP_PENETRATIONS

	if(dynamics_world){
		dynamics_world->stepSimulation(1.f/60.f,0);
	}

	int numObjects=dynamics_world->getNumCollisionObjects();
	
	for(int i=0;i<numObjects;i++){
		btCollisionObject* colObj=dynamics_world->getCollisionObjectArray()[i];
		btRigidBody* body=btRigidBody::upcast(colObj);

		if(body && body->getMotionState()){
			btDefaultMotionState* myMotionState=(btDefaultMotionState*)body->getMotionState();
			myMotionState->m_graphicsWorldTrans=myMotionState->m_startWorldTrans;

			colObj->setWorldTransform(myMotionState->m_graphicsWorldTrans );
			colObj->setInterpolationWorldTransform(myMotionState->m_startWorldTrans);
			colObj->activate();

			//Removed cached contact points
			dynamics_world->getBroadphase()->cleanProxyFromPairs(colObj->getBroadphaseHandle());

			btRigidBody* body=btRigidBody::upcast(colObj);

			if(body&&!body->isStaticObject()){
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
void PhysicsSimulator::ShootBox(const btVector3& destination)
{
	if(dynamics_world){
		float mass=1.0f;
		btTransform transform;
		transform.setIdentity();
      gadget::PositionData* head_pos;
      head_pos=head->getPositionData();
		btVector3 position;
      position.setValue(head_pos->mPosData[0][3],head_pos->mPosData[1][3],head_pos->mPosData[2][3]);
		transform.setOrigin(position);

		btCollisionShape* box_shape=new btBoxShape(btVector3(1.0f,1.0f,1.0f));
		btRigidBody* body=this->CreateRigidBody(mass,transform,box_shape);

		btVector3 lin_vel(destination[0]-position[0],destination[1]-position[1],destination[2]-position[2]);
		lin_vel.normalize();
		lin_vel*=shoot_speed;

		body->getWorldTransform().setOrigin(position);
		body->getWorldTransform().setRotation(btQuaternion(0,0,0,1));
		body->setLinearVelocity(lin_vel);
		body->setAngularVelocity(btVector3(0,0,0));
	}
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetPhysicsState(bool  state)
{
   physics=state;
}
////////////////////////////////////////////////////////////////////////////////
bool PhysicsSimulator::GetPhysicsState()
{
   return physics;
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::SetShootSpeed(float speed)
{
   shoot_speed=speed;
}
////////////////////////////////////////////////////////////////////////////////
btRigidBody* PhysicsSimulator::CreateRigidBody(float mass,const btTransform& startTransform,btCollisionShape* shape)
{
	//RigidBody is dynamic if and only if mass is non zero, otherwise static
	bool isDynamic=(mass!=0.f);

	btVector3 localInertia(0,0,0);
   if(isDynamic){
		shape->calculateLocalInertia(mass,localInertia);
   }

	//Using motionstate is recommended, it provides interpolation capabilities, and only synchronizes 'active' objects
   #ifdef USE_MOTIONSTATE
	   btDefaultMotionState* myMotionState=new btDefaultMotionState(startTransform);
	   btRigidBody* body=new btRigidBody(mass,myMotionState,shape,localInertia);
   #else
	   btRigidBody* body=new btRigidBody(mass,startTransform,shape,localInertia);	
   #endif //USE_MOTIONSTATE

	dynamics_world->addRigidBody(body);
	
	return body;
}
////////////////////////////////////////////////////////////////////////////////
btDynamicsWorld* PhysicsSimulator::GetDynamicsWorld()
{
   return dynamics_world;
}
////////////////////////////////////////////////////////////////////////////////

#endif //VE_PHYSICS