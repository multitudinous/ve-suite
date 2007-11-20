// --- My Includes --- //
#include "CoinFunnelGP.h"
#include "World.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/PhysicsSimulator.h>

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

    world = new demo::World( _dcs.get() );
}
////////////////////////////////////////////////////////////////////////////////
void CoinFunnelGP::PreFrameUpdate()
{
    if( !ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetIdle() )
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
