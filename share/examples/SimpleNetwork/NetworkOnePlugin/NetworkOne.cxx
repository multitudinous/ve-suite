#include "NetworkOne.h"
#include "NetworkOneUIDialog.h"
#include <ves/conductor/ConductorLibEnums.h>

#include "network.xpm"

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS(NetworkOne, ves::conductor::UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
NetworkOne::NetworkOne()
{
	mPluginName = wxT("NetworkOne");
	
	RegistVar("mTextOne", &mTextOne);

	wxImage my_img( network_xpm );
    SetImage( my_img );
    mDescription = _("NetworkOne");
}
///////////////////////////////////////////////////////////////////////////////
NetworkOne::~NetworkOne()
{

}
///////////////////////////////////////////////////////////////////////////////
double NetworkOne::GetVersion()
{
	double result=1.0;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
PORT NetworkOne::GetIPorts()
{
    if( inputPort.size() == 0 )
    {
        wxPoint tempPort1(GetIconImage()->GetWidth()*10/52, 
            GetIconImage()->GetHeight()*26/98); 
        AddPortToModel( tempPort1, UIPLUGINBASE_ADD_INPUT_PORT );

        wxPoint tempPort2(GetIconImage()->GetWidth()*10/52, 
            GetIconImage()->GetHeight()*74/98); 
        AddPortToModel( tempPort2, UIPLUGINBASE_ADD_INPUT_PORT );
    }
    
    return UIPluginBase::GetIPorts();
}
///////////////////////////////////////////////////////////////////////////////
PORT NetworkOne::GetOPorts()
{
    if( outputPort.size() == 0 )
    {
        wxPoint tempPort1(GetIconImage()->GetWidth()*43/52,
            GetIconImage()->GetHeight()*26/98);
        AddPortToModel( tempPort1, UIPLUGINBASE_ADD_OUTPUT_PORT );

        wxPoint tempPort2(GetIconImage()->GetWidth()*43/52,
            GetIconImage()->GetHeight()*74/98);
        AddPortToModel( tempPort2, UIPLUGINBASE_ADD_OUTPUT_PORT );
    }
    
    return UIPluginBase::GetOPorts();
}
///////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* NetworkOne::UI(wxWindow* parent)
{
	if (dlg!=NULL)
	{
		return dlg;
	}
  
	dlg = new NetworkOneUIDialog(parent, -1, serviceList,
		&mTextOne);
      
	dlg->CenterOnScreen(wxBOTH);

	return dlg;
}
///////////////////////////////////////////////////////////////////////////////
wxString NetworkOne::GetName()
{
	wxString result(_("NetworkOne")); //your name
	return result;
}
///////////////////////////////////////////////////////////////////////////////
wxString NetworkOne::GetConductorName()
{
	wxString result(_("SimpleNetwork_NetworkOne")); //your name
	return result;
}
///////////////////////////////////////////////////////////////////////////////
