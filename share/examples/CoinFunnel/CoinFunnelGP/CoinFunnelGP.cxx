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
cfdVEBaseClass(),
world( 0 )
{
    m_objectName = "CoinFunnelUI";
}
////////////////////////////////////////////////////////////////////////////////
CoinFunnelGP::~CoinFunnelGP()
{
    if( world )
    {
        delete world;
    }
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::InitializeNode( ves::xplorer::scenegraph::DCS* veworldDCS )
{
    cfdVEBaseClass::InitializeNode( veworldDCS );

    world = new demo::World( m_dcs.get(),
                             m_physicsSimulator
#ifdef VE_SOUND
                             , m_soundManager
#endif
                             );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::PreFrameUpdate()
{
    if( !m_physicsSimulator->GetIdle() )
    {
        world->PreFrameUpdate();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::UpdateParams()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::SetCurrentCommand( ves::open::xml::Command* command )
{
    if( !command )
    {
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
