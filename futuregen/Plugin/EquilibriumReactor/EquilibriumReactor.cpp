
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "EquilibriumReactor.h"


IMPLEMENT_DYNAMIC_CLASS(EquilibriumReactor, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
EquilibriumReactor
::EquilibriumReactor()
{
}



/////////////////////////////////////////////////////////////////////////////
EquilibriumReactor
::~EquilibriumReactor()
{

}

/////////////////////////////////////////////////////////////////////////////
double EquilibriumReactor::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int EquilibriumReactor::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void EquilibriumReactor::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int EquilibriumReactor::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void EquilibriumReactor::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(20, 0);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int EquilibriumReactor::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void EquilibriumReactor::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(30, 40);
}

/////////////////////////////////////////////////////////////////////////////
void EquilibriumReactor::DrawIcon(wxDC* dc)
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
UIDialog* EquilibriumReactor::UI(wxWindow* parent)
{
      
  return NULL; //EquilibrimReactor does not have a UI
}

/////////////////////////////////////////////////////////////////////////////
wxString EquilibriumReactor::GetName()
{
  wxString result="REI_LarryRuth_EquilibriumReactor"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString EquilibriumReactor::GetDesc()
{
  wxString result="EquilibriumReactor module by REI"; //your description

  return result;
}


