
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Cyclone.h"
#include "Cyclone_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Cyclone, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Cyclone
::Cyclone()
{
  RegistVar("diameter", &diameter);
  RegistVar("particle_turn_count", &particle_turn_count);
  RegistVar("velocity_heads", &velocity_heads);
  
  diameter = 2.0;
  particle_turn_count = 5.0;
  velocity_heads = 8.0;

  n_pts = 4;
  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(56,0);
  poly[2]=wxPoint(56,40);
  poly[3]=wxPoint(0,40); 
}



/////////////////////////////////////////////////////////////////////////////
Cyclone
::~Cyclone()
{

}

/////////////////////////////////////////////////////////////////////////////
double Cyclone::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Cyclone::GetNumPoly()
{
  int result=n_pts;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Cyclone::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Cyclone::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Cyclone::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Cyclone::GetNumOports()
{
  int result=2;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Cyclone::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(56, 15); 
  oports[1]=wxPoint(56, 25); 
}

/////////////////////////////////////////////////////////////////////////////
void Cyclone::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLACK_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Cyclone::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Cyclone_UI_Dialog (parent, -1,
     &diameter,
     &particle_turn_count,
     &velocity_heads);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Cyclone::GetName()
{
  wxString result="REI_LarryRuth_Cyclone"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Cyclone::GetDesc()
{
  wxString result="Cyclone Module by REI"; //your description

  return result;
}


