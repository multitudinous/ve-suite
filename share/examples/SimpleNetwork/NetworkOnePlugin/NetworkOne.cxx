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
int NetworkOne::GetNumPoly()
{
	int result=0;
	return n_pts;
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
	iports[0]=wxPoint(icon_w*10/52, icon_h*26/98); 
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
	oports[0]=wxPoint(icon_w*43/52,icon_h*74/98);    
}
///////////////////////////////////////////////////////////////////////////////
void NetworkOne::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
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


