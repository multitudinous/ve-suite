#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"

#include "btBulletDynamicsCommon.h"

using namespace VE_SceneGraph;

const int maxProxies=32766;

vprSingletonImp(PhysicsSimulator);

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
{
   
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::CleanUp()
{
   
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

   #ifdef COMPARE_WITH_QUICKSTEP
      //This is optional
	   btConstraintSolver* solver=new OdeConstraintSolver();
   #else
	   //default constraint solver
	   btSequentialImpulseConstraintSolver* solver=new btSequentialImpulseConstraintSolver;
   #endif
		
   dynamics_world=new btDiscreteDynamicsWorld(dispatcher,broadphase,solver);
	dynamics_world->setGravity(btVector3(0,-10,0));

	//dynamics_world->setDebugDrawer(&debugDrawer);
}
////////////////////////////////////////////////////////////////////////////////
btDynamicsWorld* PhysicsSimulator::GetDynamicsWorld()
{
   return dynamics_world;
}
////////////////////////////////////////////////////////////////////////////////