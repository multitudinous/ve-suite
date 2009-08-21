/*
Graphical Plugin for VascularSimViewer. Mostly coped from WarrantyToolGP.

Timothy Gundert 8/20/09
*/

#ifndef VASCULAR_SIM_VIEWER_GP_H
#define VASCULAR_SIM_VIEWER_GP_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/plugin/PluginBase.h>

// --- osg Includes --- //
#include <osg/ref_ptr>

namespace vascularsimviewer
{
class FileIOCommandHandler;
class VizCommandHandler;

class VE_USER_PLUGIN_EXPORTS VascularSimViewerGP :
    public ves::xplorer::plugin::PluginBase
{
public:

    // Constructor
    VascularSimViewerGP();
	
	// Destructor
    virtual ~VascularSimViewerGP();

    // Start Dynamic Coordinate System
    virtual void InitializeNode( osg::Group* veworldDCS );

    // Not sure
    virtual void PreFrameUpdate();
	
    //Receives Commands from CONDUCTOR
    virtual void SetCurrentCommand( ves::open::xml::CommandPtr command );

protected:

    FileIOCommandHandler* _IOCommandHandler;
    
    VizCommandHandler* _VizCommandHandler;

private:

};

CREATE_VES_XPLORER_PLUGIN_ENTRY_POINT( VascularSimViewerGP )

} //end vascularsimviewer

#endif //VASCULAR_SIM_VIEWER_GP_H
