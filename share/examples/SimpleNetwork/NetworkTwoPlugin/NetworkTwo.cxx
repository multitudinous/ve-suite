#include "NetworkTwo.h"
#include "NetworkTwoUIDialog.h"
#include "network.xpm"

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS(NetworkTwo, UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
NetworkTwo::NetworkTwo()
{
	name = wxT("NetworkTwo");
	
	RegistVar("mTextTwo", &mTextTwo);

	wxImage my_img( network_xpm );
	icon_w = my_img.GetWidth();
	icon_h = my_img.GetHeight();
	my_icon = new wxBitmap( my_img.Scale( icon_w, icon_h ) );
	n_pts = 4;

	poly[0]=wxPoint(0,0);
	poly[1]=wxPoint(icon_w,0);
	poly[2]=wxPoint(icon_w,icon_h);
	poly[3]=wxPoint(0,icon_h);
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
int NetworkTwo::GetNumPoly()
{
	int result=0;
	return n_pts;
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
	iports[0]=wxPoint(icon_w*10/52, icon_h*26/98); 
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
	oports[0]=wxPoint(icon_w*43/52,icon_h*74/98);    
}
///////////////////////////////////////////////////////////////////////////////
void NetworkTwo::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
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
wxString NetworkTwo::GetDesc()
{
	wxString result(_("NetworkTwo")); //your description
	return result;
}


