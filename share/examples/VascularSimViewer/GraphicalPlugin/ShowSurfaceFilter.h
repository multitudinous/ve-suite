#ifndef SHOW_SURFACE_FILTER_H
#define SHOW_SURFACE_FILTER_H

// --- VTK Includes --- //
#include "PolyVisualizationBase.h"

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
class vtkUGDataSet;

class ShowSurfaceFilter : public PolyVisualizationBase
{    
    public:

        //Constructor
        ShowSurfaceFilter();
        
        //Constructor
        ShowSurfaceFilter( vtkUGDataSet* inputDataSet, ves::xplorer::scenegraph::DCS* parentDCS );
    
        //Destructor
        ~ShowSurfaceFilter();
	
    private:
        
        vtkUGDataSet* mDataSet;
    
}; //end class ShowSurfaceFilter
}  //end vascularsimviewer

#endif // SHOW_SURFACE_FILTER_H
