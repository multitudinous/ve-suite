
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "AGR.h"
#include "AGR_UI.h"

IMPLEMENT_DYNAMIC_CLASS(AGR, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
AGR
::AGR()
{
  RegistVar("solv_mw", &solv_mw);
  RegistVar("solv_den", &solv_den);
  RegistVar("solv_type", &solv_type);
  RegistVar("tray_type", &tray_type);

  solv_mw = 119.16;
  solv_den = 1041.0;
  solv_type = 0;
  tray_type = 0;

}



/////////////////////////////////////////////////////////////////////////////
AGR
::~AGR()
{

}

/////////////////////////////////////////////////////////////////////////////
double AGR::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int AGR::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void AGR::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int AGR::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AGR::GetIPorts(POLY &iports)
{
  iports[0]=poly[0];
  return;
}

/////////////////////////////////////////////////////////////////////////////
int AGR::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AGR::GetOPorts(POLY &oports)
{
  oports[0]=poly[3];
}

/////////////////////////////////////////////////////////////////////////////
//void AGR::DrawIcon(wxDC* dc)
//{
  //Your implementation
//}

/////////////////////////////////////////////////////////////////////////////
UIDialog* AGR::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new AGR_UI_Dialog(parent, -1,
     &solv_mw,
     &solv_den,
     &solv_type,
     &tray_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString AGR::GetName()
{
  wxString result="REI_Components_AGR"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString AGR::GetDesc()
{
  wxString result="AGR Module by REI"; //your description

  return result;
}

wxString AGR::GetHelp()
{
  wxString result="Framework/doc/modules/AGR.html"; //your description

  return result;
}
