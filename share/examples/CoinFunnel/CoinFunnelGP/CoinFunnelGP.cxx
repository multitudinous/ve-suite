// --- My Includes --- //
#include "CoinFunnelGP.h"
#include "World.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/Sound.h>

#include <ves/open/xml/model/Model.h>

// --- Bullet Includes --- //
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

////////////////////////////////////////////////////////////////////////////////
CoinFunnelGP::CoinFunnelGP()
:
cfdVEBaseClass(),
world( 0 )
{
    _objectName = "CoinFunnelUI";
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

    m_sound = new ves::xplorer::scenegraph::Sound( _dcs.get() );
    m_sound->LoadFile( "Sounds/31295_Corsica_S_ridged_coin.wav" );

    world = new demo::World( _dcs.get(), m_physicsSimulator );
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
