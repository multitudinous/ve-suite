#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"
#include <ode/config.h>
#if defined(dDOUBLE)
#define OPAL_USE_DOUBLE 1
#endif
#include <opal.h>

using namespace VE_SceneGraph;

vprSingletonImp(PhysicsSimulator);

////////////////////////////////////////////////////////////////////////////////
PhysicsSimulator::PhysicsSimulator()
{
   simulator=opal::createSimulator();
	simulator->setGravity(opal::Vec3r(0.0, (opal::real)-9.81, 0.0));
}
////////////////////////////////////////////////////////////////////////////////
void PhysicsSimulator::CleanUp()
{
   simulator->destroy();
}
////////////////////////////////////////////////////////////////////////////////
opal::Simulator* PhysicsSimulator::GetSimulator()
{
   return this->simulator;
}
////////////////////////////////////////////////////////////////////////////////