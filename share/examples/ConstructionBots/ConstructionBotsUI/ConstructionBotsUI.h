#ifndef CONSTRUCTION_BOTS_UI_H
#define CONSTRUCTION_BOTS_UI_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

// --- wxWidgets Includes --- //
#include <wx/image.h>

// --- C/C++ Libraries --- //
#include <string>

class ConstructionBotsUI : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( ConstructionBots )

public:
    ConstructionBotsUI();

    virtual ~ConstructionBotsUI();

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

#endif //CONSTRUCTION_BOTS_UI_H
