
#ifndef PS_PLUGIN_H
#define PS_PLUGIN_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

// --- wxWidgets Includes --- //
#include <wx/event.h>

class wxMenu;

namespace ves
{
namespace conductor
{
class PSPlugin : public UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( APPlugin )

public:
    ///Constructor
    PSPlugin();

    ///
    virtual ~PSPlugin();

    ///
    virtual wxString GetConductorName();

protected:
    ///
    virtual wxMenu* GetPluginPopupMenu( wxMenu* baseMenu );

private:
    ///
    bool IsSIPOpen();

    ///
    void OnOpen( wxCommandEvent& event );

    ///
    wxMenu* m_powersimMenu;

    DECLARE_EVENT_TABLE()

};
} //end conductor
} //end ves

#endif //PS_PLUGIN_H
