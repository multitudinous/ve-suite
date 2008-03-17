#ifndef COIN_FUNNEL_GP_H
#define COIN_FUNNEL_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

// --- My Includes --- //
//use boost shared ptrs
namespace demo
{
    class World;
}

class VE_USER_PLUGIN_EXPORTS CoinFunnelGP : public ves::xplorer::plugin::PluginBase
{
public:
    CoinFunnelGP();
    virtual ~CoinFunnelGP();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

protected:
    void UpdateParams();

private:
    //demo::WorldPtr mWorld;
    demo::World* mWorld;

};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( CoinFunnelGP )

#endif //COIN_FUNNEL_GP_H
