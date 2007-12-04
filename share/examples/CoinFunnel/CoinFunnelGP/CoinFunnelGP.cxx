// --- My Includes --- //
#include "CoinFunnelGP.h"
#include "World.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/Sound.h>

#include <ves/open/xml/model/Model.h>

#include <osgAL/SoundRoot>
#include <osgAL/SoundManager>
#include <osgAL/SoundNode>
#include <osgAL/SoundState>

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

    world = new demo::World( m_dcs.get(), m_physicsSimulator, m_soundManager );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::AddSelfToSG()
{
    cfdVEBaseClass::AddSelfToSG();
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::PreFrameUpdate()
{
    if( m_physicsSimulator->GetIdle() )
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
