#ifndef PHYSICS_SIMULATOR_H
#define PHYSICS_SIMULATOR_H
/*!\file PhysicsSimulator.h
PhysicsSimulator API
*/
/*!\class VE_SceneGraph::PhysicsSimulator
* 
*/
#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

#include <gadget/Type/PositionInterface.h>

#include <LinearMath/btTransform.h>

#include <osg/ref_ptr>

class	btDynamicsWorld;
class btCollisionDispatcher;
class btOverlappingPairCache;
class btSequentialImpulseConstraintSolver;
class btRigidBody;
class btCollisionShape;

namespace VE_SceneGraph
{
   class VE_SCENEGRAPH_EXPORTS PhysicsSimulator                //: public vpr::Singleton< PhysicsSimulator >
   {
      public:
         void ExitPhysics();                                   //Functions as the destructor

         void UpdatePhysics(float dt);
         void ResetScene();

         void ShootBox(const btVector3& destination);

         void SetPhysicsState(bool state);
         bool GetPhysicsState();

         void SetShootSpeed(float speed);

         btRigidBody* CreateRigidBody(float mass,const btTransform& startTransform,btCollisionShape* shape);

         btDynamicsWorld* GetDynamicsWorld();

      private:
         PhysicsSimulator();
         ~PhysicsSimulator(){;}                                //Never gets called, don't implement
         vprSingletonHeader(PhysicsSimulator);

         void InitPhysics();

         bool physics;
         float shoot_speed;

         gadget::PositionInterface head;

         //Manages physics objects and constraints and implements update of all objects each frame
         //******************************************//
	      btDynamicsWorld* dynamics_world;

         btCollisionDispatcher* dispatcher;
         btOverlappingPairCache* broadphase;
         btSequentialImpulseConstraintSolver* solver;
         //******************************************//
   };
}

#endif //PHYSICS_SIMULATOR_H

