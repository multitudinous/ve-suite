#ifndef NETWORK_ONE_MODEL_H
#define NETWORK_ONE_MODEL_H

#include <ves/xplorer/plugin/PluginBase.h>

class VE_USER_PLUGIN_EXPORTS NetworkOneGP : public ves::xplorer::plugin::PluginBase
{
public:
	NetworkOneGP();
	virtual ~NetworkOneGP();

	virtual void InitializeNode( ves::xplorer::scenegraph::DCS* );
    virtual void CreateCustomVizFeature( int );
    virtual void SetCurrentCommand(ves::open::xml::CommandPtr command);
    virtual void PreFrameUpdate( void );
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( NetworkOneGP )

#endif //NETWORK_ONE_MODEL_H