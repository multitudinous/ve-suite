
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "CarbonBed.h"
#include "CarbonBed_UI.h"

IMPLEMENT_DYNAMIC_CLASS(CarbonBed, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
CarbonBed
::CarbonBed()
{
  RegistVar("press_drop", &press_drop);
  RegistVar("part_diam", &part_diam);
  RegistVar("bulk_density", &bulk_density);
  RegistVar("temp", &temp);
  RegistVar("press", &press);
  RegistVar("cr_time", &cr_time);
  RegistVar("porosity", &porosity);
  RegistVar("res_time", &res_time);
  RegistVar("carbon_type", &carbon_type);

  carbon_type = 0;
  press_drop = 0.26;
  part_diam = 3.38;
  bulk_density = 592.7;
  temp = 313.0;
  press = 255.0;
  cr_time = 2.0;
  porosity = 0.7;
  res_time = 20.0;

}



/////////////////////////////////////////////////////////////////////////////
CarbonBed
::~CarbonBed()
{

}

/////////////////////////////////////////////////////////////////////////////
double CarbonBed::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int CarbonBed::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void CarbonBed::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int CarbonBed::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void CarbonBed::GetIPorts(POLY &iports)
{
  iports[0] = poly[0];
  return;
}

/////////////////////////////////////////////////////////////////////////////
int CarbonBed::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void CarbonBed::GetOPorts(POLY &oports)
{
  oports[0]=poly[0];
}

/////////////////////////////////////////////////////////////////////////////
//void CarbonBed::DrawIcon(wxDC* dc)
//{
  //Your implementation
//}

/////////////////////////////////////////////////////////////////////////////
UIDialog* CarbonBed::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new CarbonBed_UI_Dialog(parent, -1,
     &press_drop,
     &part_diam,
     &bulk_density,
     &temp,
     &press,
     &cr_time,
     &porosity,
     &res_time,
     &carbon_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString CarbonBed::GetName()
{
  wxString result="REI_Components_CarbonBed"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString CarbonBed::GetDesc()
{
  wxString result="Carbon Bed Module by REI"; //your description

  return result;
}

wxString CarbonBed::GetHelp()
{
  wxString result="Framework/doc/modules/CarbonBed.html"; //your description

  return result;
}

