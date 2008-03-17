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
PluginBase(),
world( 0 )
{
    mObjectName = "ConstructionBotsUI";
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
    PluginBase::InitializeNode( veworldDCS );

    world = new Construction::World( mDCS.get(),
                                     mPhysicsSimulator
#ifdef VE_SOUND
                                   , mSoundManager
#endif
                                   );
}
////////////////////////////////////////////////////////////////////////////////
void ConstructionBotsGP::PreFrameUpdate()
{
    if( !mPhysicsSimulator->GetIdle() )
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
void ConstructionBotsGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////

