
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "GasSource.h"
#include "GasSource_UI.h"

IMPLEMENT_DYNAMIC_CLASS(GasSource, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GasSource
::GasSource()
{
  RegistVar("temp", &temp);
  RegistVar("pres", &pres);
  RegistVar("flow", &flow);
  RegistVar("p_temp", &p_temp);
  RegistVar("p_m", &p_m);
  RegistVar("mps", &mps);
  RegistVar("sv", &sv);
  RegistVar("coalcal", &coalcal);
  RegistVar("ashcal", &ashcal);
  RegistVar("ashph", &ashph);
  RegistVar("species", &species);
  RegistVar("comp", &comp);
  RegistVar("spec_frac", &spec_frac);
  RegistVar("particles", &particles);
  RegistVar("p_comp", &p_comp);
  RegistVar("p_frac", &p_frac);

  temp = 300.778;
  pres = 101325;
  flow = 75.4;

  p_temp = 400.0;
  p_m = 0;
  mps=70E-6;
  sv=25E-10;
  coalcal= 1;
  ashcal = 10;
  ashph = 7;

  //Will be replaced by code of reading a thermo file
  species.push_back("AR");
  species.push_back("N2");
  species.push_back("C2H2");
  species.push_back("CH4");
  species.push_back("O2");
  species.push_back("CO2");
  species.push_back("COS");
  species.push_back("H20");
  species.push_back("H2");

  comp.push_back("AR");
  comp.push_back("O2");
  comp.push_back("N2");

  spec_frac.push_back("0.001");
  spec_frac.push_back("0.21");
  spec_frac.push_back("0.789");

  particles.push_back("ASH");
  particles.push_back("CHAR");
  particles.push_back("COAL");
  particles.push_back("WATER");

  p_comp.push_back("ASH");
  p_comp.push_back("CHAR");
  p_comp.push_back("COAL");
  p_comp.push_back("WATER");
 
  p_frac.push_back("1.0");
  p_frac.push_back("0.0");
  p_frac.push_back("0.0");
  p_frac.push_back("0.0");

  n_pts = 4;
  poly[3]= wxPoint(20, 40);
}



/////////////////////////////////////////////////////////////////////////////
GasSource
::~GasSource()
{

}

/////////////////////////////////////////////////////////////////////////////
double GasSource::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GasSource::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void GasSource::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int GasSource::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasSource::GetIPorts(POLY &iports)
{
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GasSource::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasSource::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40, 20);
  return ;
}

/////////////////////////////////////////////////////////////////////////////
void GasSource::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREEN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GasSource::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GasSource_UI_Dialog (parent, -1,
     &temp,
     &pres,
     &flow,
     &p_temp,
     &p_m,
     &mps,
     &sv,
     &coalcal,
     &ashcal,
     &ashph,
     &species,
     &comp,
     &spec_frac,
     &particles,
     &p_comp,
     &p_frac);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasSource::GetName()
{
  wxString result="REI_Base_GasSource"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasSource::GetDesc()
{
  wxString result="Gas Source Module by REI"; //your description

  return result;
}


