#ifndef PHYSICS_SIMULATOR_H
#define PHYSICS_SIMULATOR_H

/*!\file PhysicsSimulator.h
PhysicsSimulator API
*/
/*!\class VE_Xplorer::PhysicsSimulator
* 
*/

#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS PhysicsSimulator                //: public vpr::Singleton< PhysicsSimulator >
   {
      public:
         PhysicsSimulator();
         ~PhysicsSimulator(){;}                             //Never gets called, don't implement
         vprSingletonHeader(PhysicsSimulator);

      private:

   };
}

#endif

