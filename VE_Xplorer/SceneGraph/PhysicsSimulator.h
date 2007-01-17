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

#include "LinearMath/btTransform.h"

class	btDynamicsWorld;
class btRigidBody;
class btCollisionShape;

namespace VE_SceneGraph
{
   class VE_SCENEGRAPH_EXPORTS PhysicsSimulator                //: public vpr::Singleton< PhysicsSimulator >
   {
      public:
         void ExitPhysics();                                   //Functions as the destructor

         void UpdateCallback();
         void ResetScene();

         btRigidBody* CreateRigidBody(float mass,const btTransform& startTransform,btCollisionShape* shape);

         btDynamicsWorld* GetDynamicsWorld();

      private:
         PhysicsSimulator();
         ~PhysicsSimulator(){;}                                //Never gets called, don't implement
         vprSingletonHeader(PhysicsSimulator);

         void InitPhysics();

         //Manages physics objects and constraints and implements update of all objects each frame
	      btDynamicsWorld* dynamics_world;
   };
}

#endif //PHYSICS_SIMULATOR_H

