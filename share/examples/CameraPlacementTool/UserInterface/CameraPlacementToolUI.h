#ifndef CAMERA_PLACEMENT_TOOL_UI_H
#define CAMERA_PLACEMENT_TOOL_UI_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

// --- wxWidgets Includes --- //
#include <wx/image.h>

// --- C/C++ Libraries --- //
#include <string>

namespace cpt
{

/*----------------------------------------------------------------------------*/
class CameraPlacementToolUI : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( CameraPlacementToolUI )

public:
    CameraPlacementToolUI();
    virtual ~CameraPlacementToolUI();

    virtual double GetVersion();

    virtual void DrawIcon( wxDC* dc );

    virtual int GetNumPoly();

    virtual ves::conductor::UIDialog* UI( wxWindow* parent );

    virtual wxString GetConductorName();
    virtual wxString GetName();
    virtual wxString GetDesc();

    virtual int GetNumIports();
    virtual void GetIPorts( POLY& ports );

    virtual int GetNumOports();
    virtual void GetOPorts( POLY& ports );

protected:

private:

};
/*----------------------------------------------------------------------------*/

} //end cpt

#endif //CAMERA_PLACEMENT_TOOL_UI_H
