#include "NetworkOne.h"
#include "NetworkOneUIDialog.h"
#include "network.xpm"

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS(NetworkOne, UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
NetworkOne::NetworkOne()
{
	mPluginName = wxT("NetworkOne");
	
	RegistVar("mTextOne", &mTextOne);

	wxImage my_img( network_xpm );
    SetImage( my_img );
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
int NetworkOne::GetNumIports()
{
	int result=1;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkOne::GetIPorts(POLY &iports)
{
	iports[0]=wxPoint(GetIconImage()->GetWidth()*10/52, GetIconImage()->GetHeight()*26/98); 
	return;
}
///////////////////////////////////////////////////////////////////////////////
int NetworkOne::GetNumOports()
{
	int result=0;
	return result;
}
///////////////////////////////////////////////////////////////////////////////
void NetworkOne::GetOPorts(POLY &oports)
{
	oports[0]=wxPoint(GetIconImage()->GetWidth()*43/52,GetIconImage()->GetHeight()*74/98);    
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
wxString NetworkOne::GetDesc()
{
	wxString result(_("NetworkOne")); //your description
	return result;
}


