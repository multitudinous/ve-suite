#ifndef CONSTRUCTION_BOTS_GP_H
#define CONSTRUCTION_BOTS_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

// --- My Includes --- //
namespace Construction
{
    class World;
}

class VE_USER_PLUGIN_EXPORTS ConstructionBotsGP : public ves::xplorer::plugin::PluginBase
{
public:
    ConstructionBotsGP();
    virtual ~ConstructionBotsGP();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

protected:
    void UpdateParams();

private:
    Construction::World* world;

};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( ConstructionBotsGP )

#endif //CONSTRUCTION_BOTS_GP_H