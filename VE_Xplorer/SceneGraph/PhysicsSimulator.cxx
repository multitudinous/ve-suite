//#define USE_KINEMATIC_GROUND 1
//#define PRINT_CONTACT_STATISTICS 1
//#define REGISTER_CUSTOM_COLLISION_ALGORITHM 1
//#define USER_DEFINED_FRICTION_MODEL 1
#define USE_CUSTOM_NEAR_CALLBACK 1

#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"

#include "btBulletDynamicsCommon.h"

//#include "BulletCollision/CollisionDispatch/btSphereSphereCollisionAlgorithm.h"
//#include "../Extras/AlternativeCollisionAlgorithms/BoxBoxCollisionAlgorithm.h"
//#include "BulletCollision/CollisionDispatch/btSphereTriangleCollisionAlgorithm.h"

using namespace VE_SceneGraph;

const int maxProxies=32766;

vprSingletonImp(PhysicsSimulator);

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
{
   this->InitPhysics();
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::CleanUp()
{
   ;
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
   btCollisionDispatcher* dispatcher=new btCollisionDispatcher();

   #ifdef USE_CUSTOM_NEAR_CALLBACK
	   //This is optional
	   dispatcher->setNearCallback(customNearCallback);
   #endif

   btVector3 worldAabbMin(-10000,-10000,-10000);
	btVector3 worldAabbMax(10000,10000,10000);

	btOverlappingPairCache* broadphase=new btAxisSweep3(worldAabbMin,worldAabbMax,maxProxies);
   //btOverlappingPairCache* broadphase=new btSimpleBroadphase;
	
   #ifdef REGISTER_CUSTOM_COLLISION_ALGORITHM
      //This is optional
   #endif

	//Default constraint solver
	btSequentialImpulseConstraintSolver* solver=new btSequentialImpulseConstraintSolver;
	
   dynamics_world=new btDiscreteDynamicsWorld(dispatcher,broadphase,solver);
	dynamics_world->setGravity(btVector3(0,-10,0));

	//dynamics_world->setDebugDrawer(&debugDrawer);
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::DisplayCallback()
{
   dynamics_world->updateAabbs();

   
}
////////////////////////////////////////////////////////////////////////////////
btDynamicsWorld* PhysicsSimulator::GetDynamicsWorld()
{
   return dynamics_world;
}
////////////////////////////////////////////////////////////////////////////////