#include "NetworkThree.h"
#include "NetworkThreeUIDialog.h"
#include <ves/conductor/ConductorLibEnums.h>

#include "network.xpm"

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS(NetworkThree, ves::conductor::UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
NetworkThree::NetworkThree()
{
	mPluginName = wxT("NetworkThree");
	
	RegistVar("mNetworkThreeInputs", &mNetworkThreeInputs);

	wxImage my_img( network_xpm );
    SetImage( my_img );
    
    mDescription = _("NetworkThree");
}
///////////////////////////////////////////////////////////////////////////////
NetworkThree::~NetworkThree()
{

}
///////////////////////////////////////////////////////////////////////////////
double NetworkThree::GetVersion()
{
	double result=1.0;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
PORT NetworkThree::GetIPorts()
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
PORT NetworkThree::GetOPorts()
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
ves::conductor::UIDialog* NetworkThree::UI(wxWindow* parent)
{
	if (dlg!=NULL)
	{
		return dlg;
	}
  
	dlg = new NetworkThreeUIDialog(parent, -1, serviceList,
		&mNetworkThreeInputs);
      
	dlg->CenterOnScreen(wxBOTH);

	return dlg;
}
///////////////////////////////////////////////////////////////////////////////
wxString NetworkThree::GetName()
{
	wxString result(_("NetworkThree")); //your name
	return result;
}
///////////////////////////////////////////////////////////////////////////////
wxString NetworkThree::GetConductorName()
{
	wxString result(_("SimpleNetwork_NetworkThree")); //your name
	return result;
}
///////////////////////////////////////////////////////////////////////////////
