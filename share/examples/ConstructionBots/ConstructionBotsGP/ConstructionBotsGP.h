#ifndef CONSTRUCTION_BOTS_GP_H
#define CONSTRUCTION_BOTS_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

// --- My Includes --- //
namespace Construction
{
    class World;
}

class VE_USER_PLUGIN_EXPORTS ConstructionBotsGP : public ves::xplorer::plugin::cfdVEBaseClass
{
public:
    ConstructionBotsGP();
    virtual ~ConstructionBotsGP();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::Command* command );

protected:
    void UpdateParams();

private:
    Construction::World* world;

};

extern "C"
{
    VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
    {
        return new ConstructionBotsGP();
    }
}

#endif //CONSTRUCTION_BOTS_GP_H