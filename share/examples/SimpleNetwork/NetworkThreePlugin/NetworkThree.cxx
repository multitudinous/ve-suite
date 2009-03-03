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
int NetworkThree::GetNumIports()
{
	int result=1;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThree::GetIPorts(PORT& iports)
{
    if( inputPort.size() == 0 )
    {
        wxPoint tempPort(GetIconImage()->GetWidth()*10/52, 
            GetIconImage()->GetHeight()*26/98);
        AddPortToModel( tempPort, UIPLUGINBASE_ADD_INPUT_PORT );
    }
    
    UIPluginBase::GetIPorts( iports );
}
///////////////////////////////////////////////////////////////////////////////
int NetworkThree::GetNumOports()
{
	int result=1;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThree::GetOPorts(PORT& oports)
{
    if( outputPort.size() == 0 )
    {
        wxPoint tempPort(GetIconImage()->GetWidth()*43/52,
            GetIconImage()->GetHeight()*74/98);
        AddPortToModel( tempPort, UIPLUGINBASE_ADD_OUTPUT_PORT );
    }
    
    UIPluginBase::GetOPorts( oports );
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
