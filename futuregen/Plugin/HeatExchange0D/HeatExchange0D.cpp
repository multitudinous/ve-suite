
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "HeatExchange0D.h"
#include "HeatExchange0D_UI.h"

IMPLEMENT_DYNAMIC_CLASS(HeatExchange0D, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
HeatExchange0D
::HeatExchange0D()
{
  RegistVar("desired_temp", &desired_temp);
  desired_temp = 1000;
  
  wxString icon_file="Icons/hrsg.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
}

/////////////////////////////////////////////////////////////////////////////
HeatExchange0D
::~HeatExchange0D()
{

}

/////////////////////////////////////////////////////////////////////////////
double HeatExchange0D::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int HeatExchange0D::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void HeatExchange0D::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int HeatExchange0D::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchange0D::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,icon_h/2);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int HeatExchange0D::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchange0D::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w, icon_h/2);
}

/////////////////////////////////////////////////////////////////////////////
void HeatExchange0D::DrawIcon(wxDC* dc)
{
  /*wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLACK_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  */
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* HeatExchange0D::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new HeatExchange0D_UI_Dialog (parent, -1,
     &desired_temp);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString HeatExchange0D::GetName()
{
  wxString result="REI_LarryRuth_HeatExchange0D"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString HeatExchange0D::GetDesc()
{
  wxString result="Heat Exchanger 0D Module by REI"; //your description

  return result;
}


