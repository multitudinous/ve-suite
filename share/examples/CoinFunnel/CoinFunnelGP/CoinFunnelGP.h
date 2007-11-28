#ifndef COIN_FUNNEL_GP_H
#define COIN_FUNNEL_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
    class Sound;
}
}
}

// --- My Includes --- //
namespace demo
{
    class World;
}

class VE_USER_PLUGIN_EXPORTS CoinFunnelGP : public ves::xplorer::plugin::cfdVEBaseClass
{
public:
    CoinFunnelGP();
    virtual ~CoinFunnelGP();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::Command* command );

protected:
    void UpdateParams();

private:
    demo::World* world;
    ves::xplorer::scenegraph::Sound* m_sound;

};

extern "C"
{
    VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
    {
        return new CoinFunnelGP();
    }
}

#endif //COIN_FUNNEL_GP_H