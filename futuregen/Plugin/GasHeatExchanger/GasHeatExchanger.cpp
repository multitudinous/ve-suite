
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "GasHeatExchanger.h"
#include "GasHeatExchanger_UI.h"

IMPLEMENT_DYNAMIC_CLASS(GasHeatExchanger, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GasHeatExchanger
::GasHeatExchanger()
{
  RegistVar("desired_temp", &desired_temp);

  desired_temp = 1000;
}



/////////////////////////////////////////////////////////////////////////////
GasHeatExchanger
::~GasHeatExchanger()
{

}

/////////////////////////////////////////////////////////////////////////////
double GasHeatExchanger::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GasHeatExchanger::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void GasHeatExchanger::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int GasHeatExchanger::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasHeatExchanger::GetIPorts(POLY &iports)
{
  iports[0]=poly[1];
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GasHeatExchanger::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasHeatExchanger::GetOPorts(POLY &oports)
{
  oports[0]=poly[3];
}

/////////////////////////////////////////////////////////////////////////////
void GasHeatExchanger::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GasHeatExchanger::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GasHeatExchanger_UI_Dialog (parent, -1,
     &desired_temp);
  
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasHeatExchanger::GetName()
{
  wxString result="REI_Gas_GasHeatExchanger"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasHeatExchanger::GetDesc()
{
  wxString result="GasHeatExchanger Module by REI"; //your description

  return result;
}


