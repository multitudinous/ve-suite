#ifndef NETWORK_ONE_H
#define NETWORK_ONE_H

#include <ves/conductor/UIPluginBase.h>

#include <wx/image.h>

class NetworkOne : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( NetworkOne )

public:
    NetworkOne();
    virtual ~NetworkOne();

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

#endif //NETWORK_ONE_PLUGIN_H