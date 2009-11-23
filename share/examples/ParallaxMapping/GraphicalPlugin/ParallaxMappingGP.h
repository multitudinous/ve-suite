
#ifndef PARALLAX_MAPPING_GP_H
#define PARALLAX_MAPPING_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class CADEntity;
} //end scenegraph
} //end xplorer
} //end ves

class VE_USER_PLUGIN_EXPORTS ParallaxMappingGP :
    public ves::xplorer::plugin::PluginBase
{
public:
    ParallaxMappingGP();

    virtual ~ParallaxMappingGP();

    virtual void InitializeNode( osg::Group* veworldDCS );

    virtual void PreFrameUpdate();

    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

protected:

private:
    ves::xplorer::scenegraph::CADEntity* m_cadEntity;
};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( ParallaxMappingGP )

#endif //PARALLAX_MAPPING_GP_H

