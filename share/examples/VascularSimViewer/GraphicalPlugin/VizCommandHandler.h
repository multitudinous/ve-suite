#ifndef VIZ_COMMAND_HANDLER_H
#define VIZ_COMMAND_HANDLER_H

// --- VE-Suite Includes --- //
#include <ves/open/xml/CommandPtr.h>

// --- C++ Includes --- //
#include <vector> //I include this for debugging

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
}
}
}

namespace vascularsimviewer
{
class PolyVisualizationBase;
class vtkUGDataSet;

class VizCommandHandler
{
public:
    VizCommandHandler( ves::xplorer::scenegraph::DCS* parentDCS );
    ~VizCommandHandler();

    void ProcessCurrentCommand( ves::open::xml::CommandPtr command );
    
    void CreateVisualization( vtkUGDataSet* inDataSet, const std::string& inCommand );
    
protected:

private:

    ves::xplorer::scenegraph::DCS* mPluginDCS;

    std::vector< PolyVisualizationBase* > mVisualizations;
};

} //end vascularsimviewer

#endif //VIZ_COMMAND_HANDLER_H
