#include "NetworkThree.h"
#include "NetworkThreeUIDialog.h"
#include "network.xpm"

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS(NetworkThree, UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
NetworkThree::NetworkThree()
{
	mPluginName = wxT("NetworkThree");
	
	RegistVar("mTextThree", &mTextThree);

	wxImage my_img( network_xpm );
    SetImage( my_img );
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
void NetworkThree::GetIPorts(POLY &iports)
{
	iports[0]=wxPoint(GetIconImage()->GetWidth()*10/52, GetIconImage()->GetHeight()*26/98); 
	return;
}
///////////////////////////////////////////////////////////////////////////////
int NetworkThree::GetNumOports()
{
	int result=0;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkThree::GetOPorts(POLY &oports)
{
	oports[0]=wxPoint(GetIconImage()->GetWidth()*43/52,GetIconImage()->GetHeight()*74/98);    
}
///////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* NetworkThree::UI(wxWindow* parent)
{
	if (dlg!=NULL)
	{
		return dlg;
	}
  
	dlg = new NetworkThreeUIDialog(parent, -1, serviceList,
		&mTextThree);
      
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
wxString NetworkThree::GetDesc()
{
	wxString result(_("NetworkThree")); //your description
	return result;
}


