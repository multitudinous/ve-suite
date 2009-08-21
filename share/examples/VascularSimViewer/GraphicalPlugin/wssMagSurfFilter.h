#ifndef WSS_MAG_SURF_FILTER_H
#define WSS_MAG_SURF_FILTER_H

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

class wssMagSurfFilter : public PolyVisualizationBase
{    
    public:

        //Constructor
        wssMagSurfFilter();
        
        //Constructor
        wssMagSurfFilter( vtkUGDataSet* inputDataSet, ves::xplorer::scenegraph::DCS* parentDCS );
    
        //Destructor
        ~wssMagSurfFilter();
	
    private:
        
        vtkUGDataSet* mDataSet;
    
}; //end class ShowSurfaceFilter
}  //end vascularsimviewer

#endif // WSS_MAG_SURF_FILTER_H
