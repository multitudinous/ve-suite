// --- My Includes --- //
#include "CoinFunnelGP.h"
#include "World.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

#include <ves/open/xml/model/Model.h>

// --- osgAL Includes --- //
#ifdef VE_SOUND
#include <osgAL/SoundRoot>
#include <osgAL/SoundManager>
#include <osgAL/SoundNode>
#include <osgAL/SoundState>
#endif

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

#include <osgUtil/Optimizer>

////////////////////////////////////////////////////////////////////////////////
CoinFunnelGP::CoinFunnelGP()
:
PluginBase(),
mWorld( 0 )
{
    mObjectName = "CoinFunnelUI";
}
////////////////////////////////////////////////////////////////////////////////
CoinFunnelGP::~CoinFunnelGP()
{
    if( mWorld )
    {
        delete mWorld;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    mWorld = new demo::World( mDCS.get(),
                              mPhysicsSimulator
#ifdef VE_SOUND
                            , mSoundManager
#endif
                             );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::PreFrameUpdate()
{
    if( !mPhysicsSimulator->GetIdle() )
    {
        mWorld->PreFrameUpdate();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::UpdateParams()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
