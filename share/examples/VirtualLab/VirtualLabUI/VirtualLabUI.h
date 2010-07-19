#ifndef VIRTUAL_LAB_H
#define VIRTUAL_LAB_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIPluginBase.h>

#include <wx/image.h>

class VirtualLabUI : public ves::conductor::UIPluginBase
{
	DECLARE_DYNAMIC_CLASS(VirtualLabUI)

public:
    VirtualLabUI();
    virtual ~VirtualLabUI();

    virtual double GetVersion();

    virtual ves::conductor::UIDialog* UI( wxWindow* parent );

    virtual wxString GetConductorName();
    virtual wxString GetName();

    virtual PORT GetIPorts();

    virtual PORT GetOPorts();

	std::string mTextOne;

protected:

private:

};

#endif //VIRTUAL_LAB_H


