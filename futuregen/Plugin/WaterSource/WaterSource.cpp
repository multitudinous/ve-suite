
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "WaterSource.h"
#include "WaterSource_UI.h"

IMPLEMENT_DYNAMIC_CLASS(WaterSource, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
WaterSource
::WaterSource()
{
  RegistVar("temp", &temp);
  RegistVar("pres", &pres);
  RegistVar("enth", &enth);
  RegistVar("flow", &flow);
  RegistVar("case_type", &case_type);

  temp = 499.667;
  pres = 11721500;
  enth = 0;
  flow = 78;

  n_pts = 4;
  poly[3]= wxPoint(20, 40);
}



/////////////////////////////////////////////////////////////////////////////
WaterSource
::~WaterSource()
{

}

/////////////////////////////////////////////////////////////////////////////
double WaterSource::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int WaterSource::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void WaterSource::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int WaterSource::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void WaterSource::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int WaterSource::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void WaterSource::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40, 20);
  return ;
}

/////////////////////////////////////////////////////////////////////////////
void WaterSource::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLUE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* WaterSource::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new WaterSource_UI_Dialog (parent, -1,
     &temp,
     &pres,
     &enth,
     &flow,
     &case_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString WaterSource::GetName()
{
  wxString result="REI_Water_WaterSource"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString WaterSource::GetDesc()
{
  wxString result="Water Source Module by REI"; //your description

  return result;
}


