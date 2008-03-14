#ifndef COIN_FUNNEL_GP_H
#define COIN_FUNNEL_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

// --- My Includes --- //
#if 0
use boost shared ptrs
namespace demo
{
    class World;
}
#endif

class VE_USER_PLUGIN_EXPORTS CoinFunnelGP : public ves::xplorer::plugin::cfdVEBaseClass
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
    demo::WorldPtr mWorld;

};

#if 0
use provided macro
extern "C"
{
    VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
    {
        return new CoinFunnelGP();
    }
}
#endif

#endif //COIN_FUNNEL_GP_H
