
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

  n_pts = 4;
  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(56,0);
  poly[2]=wxPoint(56,40);
  poly[3]=wxPoint(0,40);
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
  iports[0]=wxPoint(0,20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int V21ASU::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void V21ASU::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(56, 20);
}

/////////////////////////////////////////////////////////////////////////////
void V21ASU::DrawIcon(wxDC* dc)
{
  //Your implementation
  //printf("Draw here\n");
  
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREEN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  
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
  wxString result=_T("REI_LarryRuth_ASU"); //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString V21ASU::GetDesc()
{
  wxString result=_T("Air Seperation Unit for Larry Ruth Configuration by REI"); //your description

  return result;
}


