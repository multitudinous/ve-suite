#ifndef VIRTUAL_LAB_GP_H
#define VIRTUAL_LAB_GP_H

#include <ves/xplorer/plugin/PluginBase.h>

class VE_USER_PLUGIN_EXPORTS VirtualLabGP : public ves::xplorer::plugin::PluginBase
{
public:
	VirtualLabGP();
	virtual ~VirtualLabGP();

	virtual void InitializeNode( osg::Group* );
    virtual void CreateCustomVizFeature( int );
    virtual void SetCurrentCommand(ves::open::xml::CommandPtr command);
    virtual void PreFrameUpdate( void );
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( VirtualLabGP )

#endif //VE_SUSPENSION_CONFIGURATOR_H
      
