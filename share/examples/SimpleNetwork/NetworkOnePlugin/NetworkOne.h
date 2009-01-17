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

    virtual int GetNumIports();
    virtual void GetIPorts( PORT& ports );

    virtual int GetNumOports();
    virtual void GetOPorts( PORT& ports );

	std::string mTextOne;

protected:

private:

};

#endif //NETWORK_ONE_PLUGIN_H