#ifndef NETWORK_TWO_H
#define NETWORK_TWO_H

#include <ves/conductor/UIPluginBase.h>

#include <wx/image.h>

class NetworkTwo : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( NetworkTwo )

public:
    NetworkTwo();
    virtual ~NetworkTwo();

    virtual double GetVersion();

    virtual ves::conductor::UIDialog* UI( wxWindow* parent );

    virtual wxString GetConductorName();
    virtual wxString GetName();

    virtual PORT GetIPorts();

    virtual PORT GetOPorts();

	std::string mTextTwo;

protected:

private:

};

#endif //NETWORK_TWO_PLUGIN_H