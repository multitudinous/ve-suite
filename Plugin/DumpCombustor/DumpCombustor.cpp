
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "DumpCombustor.h"
#include "DumpCombustor_UI.h"

IMPLEMENT_DYNAMIC_CLASS(DumpCombustor, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
DumpCombustor
::DumpCombustor()
{
  RegistVar("desired_temp", &desired_temp);
  RegistVar("air_temp", &air_temp);
  RegistVar("air_humidity", &air_humidity);
  RegistVar("ambient_pres", &ambient_pres);

  desired_temp = 305;
  air_temp = 298;
  air_humidity = 20;
  ambient_pres = 101325;
}



/////////////////////////////////////////////////////////////////////////////
DumpCombustor
::~DumpCombustor()
{

}

/////////////////////////////////////////////////////////////////////////////
double DumpCombustor::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int DumpCombustor::GetNumPoly()
{
  int result=n_pts;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void DumpCombustor::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int DumpCombustor::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void DumpCombustor::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int DumpCombustor::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void DumpCombustor::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40,20);
}

/////////////////////////////////////////////////////////////////////////////
void DumpCombustor::DrawIcon(wxDC* dc)
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
UIDialog* DumpCombustor::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new DumpCombustor_UI_Dialog (parent, -1,
     &desired_temp,
     &air_temp,
     &air_humidity,
     &ambient_pres);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString DumpCombustor::GetName()
{
  wxString result="REI_LarryRuth_DumpCombustor"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString DumpCombustor::GetDesc()
{
  wxString result="Dump Combustor Module by REI"; //your description

  return result;
}


