#ifndef GRAPHICAL_PLUGIN_H
#define GRAPHICAL_PLUGIN_H

// --- My Includes --- //

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/cfdVEBaseClass.h>

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

};

extern "C"
{
    VE_USER_PLUGIN_EXPORTS void* CreateVEPlugin()
    {
        return new GraphicalPlugin();
    }
}

#endif //GRAPHICAL_PLUGIN_H