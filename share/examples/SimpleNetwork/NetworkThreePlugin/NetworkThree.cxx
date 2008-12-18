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
int NetworkThree::GetNumPoly()
{
	int result=0;
	return n_pts;
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
	iports[0]=wxPoint(icon_w*10/52, icon_h*26/98); 
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
	oports[0]=wxPoint(icon_w*43/52,icon_h*74/98);    
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


