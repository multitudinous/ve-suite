
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "GasSeparator.h"
#include "GasSeparator_UI.h"

IMPLEMENT_DYNAMIC_CLASS(GasSeparator, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GasSeparator
::GasSeparator()
{
  RegistVar("purity", &purity);
  RegistVar("remain", &remain);
  RegistVar("specie", &specie);

  specie="";
  remain = 1.0;
  purity = 95.0;
}



/////////////////////////////////////////////////////////////////////////////
GasSeparator
::~GasSeparator()
{

}

/////////////////////////////////////////////////////////////////////////////
double GasSeparator::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GasSeparator::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void GasSeparator::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int GasSeparator::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasSeparator::GetIPorts(POLY &iports)
{
  iports[0] = poly[0];
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GasSeparator::GetNumOports()
{
  int result=2;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasSeparator::GetOPorts(POLY &oports)
{
  oports[0]=poly[2];
  oports[1]=poly[3];
}

/////////////////////////////////////////////////////////////////////////////
void GasSeparator::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GasSeparator::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GasSeparator_UI_Dialog (parent, -1,
     &purity,
     &remain,
     &specie);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasSeparator::GetName()
{
  wxString result="REI_Gas_GasSeparator"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasSeparator::GetDesc()
{
  wxString result="Gas Separator Module by REI"; //your description

  return result;
}


