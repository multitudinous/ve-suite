// --- My Includes --- //
#include "ConstructionBotsGP.h"
#include "World.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

////////////////////////////////////////////////////////////////////////////////
ConstructionBotsGP::ConstructionBotsGP()
:
cfdVEBaseClass(),
world( 0 )
{
    m_objectName = "ConstructionBotsUI";
}
////////////////////////////////////////////////////////////////////////////////
ConstructionBotsGP::~ConstructionBotsGP()
{
    if( world )
    {
        delete world;
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsGP::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    cfdVEBaseClass::InitializeNode( veworldDCS );

    world = new Construction::World( 1, 
                                     m_dcs.get(),
                                     m_physicsSimulator
#ifdef VE_SOUND
                                   , m_soundManager
#endif
                                   );
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsGP::PreFrameUpdate()
{
    if( !ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetIdle() )
    {
        world->PreFrameUpdate();
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsGP::UpdateParams()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsGP::SetCurrentCommand( ves::open::xml::Command* command )
{
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////

