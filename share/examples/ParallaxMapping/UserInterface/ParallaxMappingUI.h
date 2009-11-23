
#ifndef PARALLAX_MAPPING_UI_H
#define PARALLAX_MAPPING_UI_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

class ParallaxMappingUI : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( ParallaxMappingUI )

public:
    ParallaxMappingUI();

    virtual ~ParallaxMappingUI();

    virtual double GetVersion();

    virtual ves::conductor::UIDialog* UI( wxWindow* parent );

    virtual wxString GetConductorName();

    virtual wxString GetName();

    virtual PORT GetIPorts();

    virtual PORT GetOPorts();

protected:

private:

};

#endif //PARALLAX_MAPPING_UI_H
