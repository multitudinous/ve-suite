#include "IntStoves.h"
#include "IntStoves_UI_Dialog.h"
#include "plancha.xpm"

#include <ves/open/xml/DataValuePair.h>

#include <wx/dc.h>

IMPLEMENT_DYNAMIC_CLASS(IntStoves, UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
IntStoves
::IntStoves()
{
    numbaffles = 0;
  RegistVar("numbaffles", &numbaffles);
  RegistVar("baffle1", &baffle1);
  RegistVar("baffle2", &baffle2);
  RegistVar("baffle3", &baffle3);
  RegistVar("baffle4", &baffle4);
  RegistVar("baffle5", &baffle5);
  RegistVar("baffle6", &baffle6);
  RegistVar("baffle7", &baffle7);

  //wxString icon_file="Icons/Plancha.gif";
   wxImage my_img( plancha_xpm );
   icon_w = static_cast<int>(my_img.GetWidth()*0.7);
   icon_h = static_cast<int>(my_img.GetHeight()*0.7);
   my_icon = new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);

  name = _("IntStoves");
}



/////////////////////////////////////////////////////////////////////////////
IntStoves
::~IntStoves()
{

}

/////////////////////////////////////////////////////////////////////////////
double IntStoves::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int IntStoves::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void IntStoves::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int IntStoves::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void IntStoves::GetIPorts(POLY &iports)
{
  //iports[0]=wxPoint(icon_w*10/52, icon_h*26/98);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int IntStoves::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void IntStoves::GetOPorts(POLY &oports)
{
    oports[0]=wxPoint(icon_w*40/43,icon_h*21/41);  
}

/////////////////////////////////////////////////////////////////////////////
void IntStoves::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* IntStoves::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new IntStoves_UI_Dialog(parent, -1, serviceList,
	 &numbaffles,
     &baffle1,
     &baffle2,
     &baffle3,
     &baffle4,
     &baffle5,
     &baffle6,
     &baffle7);

	dlg->CenterOnScreen(wxBOTH);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString IntStoves::GetName()
{
  wxString result( _("IntStoves") ); //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString IntStoves::GetDesc()
{
  wxString result( _("IntStoves") ); //your description

  return result;
}
///////////////////////////////////////////////////////////////////////////////
wxString IntStoves::GetConductorName()
{         
   wxString result( _("IntStoves") );
   return result;
}
