
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "V21ASU.h"
#include "V21ASU_UI.h"

IMPLEMENT_DYNAMIC_CLASS(V21ASU, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
V21ASU
::V21ASU()
{
  RegistVar("o2_temp", &o2_temp);
  RegistVar("o2_pres", &o2_pres);
  RegistVar("o2_purity", &o2_purity);
  RegistVar("n2_temp", &n2_temp);
  RegistVar("n2_pres", &n2_pres);
  
  o2_temp = 225;
  o2_pres = 506625;
  o2_purity = 95;
  n2_temp = 298.15;
  n2_pres = 10132;

  wxString icon_file="Icons/ASU.gif";
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
V21ASU
::~V21ASU()
{

}

/////////////////////////////////////////////////////////////////////////////
double V21ASU::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int V21ASU::GetNumPoly()
{
  int result=n_pts;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void V21ASU::GetPoly(POLY &polygon)
//{
//  GetPoly(polygon);
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int V21ASU::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void V21ASU::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*0/54,icon_h*37/43);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int V21ASU::GetNumOports()
{
  int result=2;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void V21ASU::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*53/54, icon_h*32/43);
  oports[1]=wxPoint(icon_w*53/54, icon_h*40/43);
}

/////////////////////////////////////////////////////////////////////////////
void V21ASU::DrawIcon(wxDC* dc)
{
  //Your implementation
  //printf("Draw here\n");
  /*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREEN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
  
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* V21ASU::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new V21ASU_UI_Dialog (parent, -1,
     &o2_temp,
     &o2_pres,
     &o2_purity,
     &n2_temp,
     &n2_pres);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString V21ASU::GetName()
{
  wxString result=_T("REI_Components_ASU"); //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString V21ASU::GetDesc()
{
  wxString result=_T("Air Seperation Unit for Larry Ruth Configuration by REI"); //your description

  return result;
}

wxString V21ASU::GetHelp()
{
  wxString result="Framework/doc/modules/ASU.html"; //your description

  return result;
}

