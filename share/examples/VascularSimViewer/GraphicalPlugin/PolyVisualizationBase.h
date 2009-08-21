/*
Included this base class in case I want to add any
features to all of my poly data visualizations
*/

#ifndef VISUALIZATION_BASE_H
#define VISUALIZATION_BASE_H

// --- VTK Includes --- //
#include <vtkPolyDataAlgorithm.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class Geode;
}
}
}

namespace vascularsimviewer
{
class PolyVisualizationBase : public vtkPolyDataAlgorithm
{
    public:
        PolyVisualizationBase();

        ~PolyVisualizationBase();
       
    protected:
    
        ///A pointer to the plugin DCS
        ves::xplorer::scenegraph::DCS* mPluginDCS;
    
        ves::xplorer::scenegraph::Geode* mGeode;
};

} //end vascularsimviewer

#endif // VISUALIZATION_BASE_H
