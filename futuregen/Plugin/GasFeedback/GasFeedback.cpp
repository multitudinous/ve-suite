
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "GasFeedback.h"
#include "GasFeedback_UI.h"
#include "thermo.h"

IMPLEMENT_DYNAMIC_CLASS(GasFeedback, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GasFeedback
::GasFeedback()
{
  RegistVar("iterations", &iterations);
  RegistVar("species", &species);
  RegistVar("sel_species", &sel_species);
  RegistVar("max_error", &max_error);
  
  iterations = 1;
  species.push_back("Temperature");
  species.push_back("Pressure");
  species.push_back("Flowrate");
  species.push_back("Particle Flowrate");

  n_pts = 4;
  poly[3]= wxPoint(20, 40);

  thermo *thrmo;
  std::string therm_path = "therm";
  thrmo = new thermo(therm_path);
    
  const std::map<std::string, int>& name_map = thrmo->get_nam_spec();
  
  map<std::string, int>::const_iterator iter;
  for(iter=name_map.begin(); iter!=name_map.end(); iter++)
    species.push_back((iter->first).c_str());

  delete thrmo;
}



/////////////////////////////////////////////////////////////////////////////
GasFeedback
::~GasFeedback()
{

}

/////////////////////////////////////////////////////////////////////////////
double GasFeedback::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GasFeedback::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void GasFeedback::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int GasFeedback::GetNumIports()
{
  int result=2;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasFeedback::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0, 20);
  iports[1]=wxPoint(20, 0);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GasFeedback::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasFeedback::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40, 20);
}

/////////////////////////////////////////////////////////////////////////////
void GasFeedback::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxCYAN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GasFeedback::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GasFeedback_UI_Dialog (parent, -1,
     &iterations,
     &species,
     &sel_species,
     &max_error);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasFeedback::GetName()
{
  wxString result="REI_Gas_GasFeedback"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasFeedback::GetDesc()
{
  wxString result="Gas Feedback Module by REI"; //your description

  return result;
}


