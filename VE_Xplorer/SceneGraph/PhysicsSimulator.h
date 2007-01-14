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

/*namespace opal
{
   class Simulator;
}*/

namespace VE_SceneGraph
{
   class VE_SCENEGRAPH_EXPORTS PhysicsSimulator                //: public vpr::Singleton< PhysicsSimulator >
   {
      public:
         void CleanUp();                                       //Functions like a destructor

         //opal::Simulator* GetSimulator();

      private:
         PhysicsSimulator();
         ~PhysicsSimulator(){;}                                //Never gets called, don't implement
         vprSingletonHeader(PhysicsSimulator);


         //opal::Simulator* simulator;
   };
}

#endif

