#ifndef GRAPHICAL_PLUGIN_H
#define GRAPHICAL_PLUGIN_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

namespace cpt
{
// --- My Includes --- //
class Scene;

/*----------------------------------------------------------------------------*/
class VE_USER_PLUGIN_EXPORTS GraphicalPlugin : public ves::xplorer::plugin::cfdVEBaseClass
{
public:
    GraphicalPlugin();
    virtual ~GraphicalPlugin();
    virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );
    virtual void PreFrameUpdate();
    virtual void SetCurrentCommand( ves::open::xml::Command* command );

protected:
    void UpdateParams();

private:
    cpt::Scene* mScene;
};
/*----------------------------------------------------------------------------*/

#if 0


THERE IS A MACRO FOR THIS IN THE BASE CLASS

extern "C"
{
    VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
    {
        return new GraphicalPlugin();
    }
}
} //end cpt
#endif

#endif //GRAPHICAL_PLUGIN_H
