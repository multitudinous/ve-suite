/*
This is the UI for the Vascular Sim Veiwer Plugin.
The purpose of the Plugin is to visualize the results 
of arterial flow simulations.

Timothy Gundert - 8/20/09
*/

#ifndef VASCULAR_SIM_VIEWER_UI_H
#define VASCULAR_SIM_VIEWER_UI_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

namespace vascularsimviewer
{
class VascularSimViewerUI : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( VascularSimViewerUI )

    // Most of this class was copied from the Warranty Tool Plugin
    
    public:
        // Constructor
        VascularSimViewerUI();
        
        // Destructor
        virtual ~VascularSimViewerUI();

        virtual double GetVersion();

        virtual ves::conductor::UIDialog* UI( wxWindow* parent );

        virtual wxString GetConductorName();
        virtual wxString GetName();

    protected:

    private:

};
} //end

#endif //VASCULAR_SIM_VIEWER_UI_H
