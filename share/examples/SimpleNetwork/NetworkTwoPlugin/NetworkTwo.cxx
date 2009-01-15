#include "NetworkTwo.h"
#include "NetworkTwoUIDialog.h"
#include "network.xpm"

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS(NetworkTwo, UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
NetworkTwo::NetworkTwo()
{
	mPluginName = wxT("NetworkTwo");
	
	RegistVar("mTextTwo", &mTextTwo);

	wxImage my_img( network_xpm );
    SetImage( my_img );
    
    mDescription = _("NetworkTwo");
}
///////////////////////////////////////////////////////////////////////////////
NetworkTwo::~NetworkTwo()
{

}
///////////////////////////////////////////////////////////////////////////////
double NetworkTwo::GetVersion()
{
	double result=1.0;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
int NetworkTwo::GetNumIports()
{
	int result=1;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwo::GetIPorts(POLY &iports)
{
	iports[0]=wxPoint(GetIconImage()->GetWidth()*10/52, GetIconImage()->GetHeight()*26/98); 
	return;
}
///////////////////////////////////////////////////////////////////////////////
int NetworkTwo::GetNumOports()
{
	int result=0;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwo::GetOPorts(POLY &oports)
{
	oports[0]=wxPoint(GetIconImage()->GetWidth()*43/52,GetIconImage()->GetHeight()*74/98);    
}
///////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* NetworkTwo::UI(wxWindow* parent)
{
	if (dlg!=NULL)
	{
		return dlg;
	}
  
	dlg = new NetworkTwoUIDialog(parent, -1, serviceList,
		&mTextTwo);
      
	dlg->CenterOnScreen(wxBOTH);

	return dlg;
}
///////////////////////////////////////////////////////////////////////////////
wxString NetworkTwo::GetName()
{
	wxString result(_("NetworkTwo")); //your name
	return result;
}
///////////////////////////////////////////////////////////////////////////////
wxString NetworkTwo::GetConductorName()
{
	wxString result(_("SimpleNetwork_NetworkTwo")); //your name
	return result;
}
///////////////////////////////////////////////////////////////////////////////
