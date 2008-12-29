#ifndef NETWORK_THREE_H
#define NETWORK_THREE_H

#include <ves/conductor/UIPluginBase.h>

#include <wx/image.h>

class NetworkThree : public ves::conductor::UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( NetworkThree )

public:
    NetworkThree();
    virtual ~NetworkThree();

    virtual double GetVersion();
    
    virtual ves::conductor::UIDialog* UI( wxWindow* parent );

    virtual wxString GetConductorName();
    virtual wxString GetName();
    virtual wxString GetDesc();

    virtual int GetNumIports();
    virtual void GetIPorts( POLY& ports );

    virtual int GetNumOports();
    virtual void GetOPorts( POLY& ports );

	std::string mTextThree;

protected:

private:

};

#endif //NETWORK_THREE_PLUGIN_H