
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "CatalyticConverter.h"
#include "CatalyticConverter_UI.h"

IMPLEMENT_DYNAMIC_CLASS(CatalyticConverter, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
CatalyticConverter
::CatalyticConverter()
{
  RegistVar("effect", &effect);
  RegistVar("site_den", &site_den);
  RegistVar("hydDiameter", &hydDiameter);
  RegistVar("length", &length);
  RegistVar("conversion", &conversion);
  RegistVar("velocity", &velocity);
  RegistVar("case_type", &case_type);

  effect = 1.0;
  site_den = 2.7e-5;
  hydDiameter = 0.0012;
  length = 0.078;
  conversion = 0.99;
  velocity = 16.7;
  case_type = 0;

}



/////////////////////////////////////////////////////////////////////////////
CatalyticConverter
::~CatalyticConverter()
{

}

/////////////////////////////////////////////////////////////////////////////
double CatalyticConverter::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int CatalyticConverter::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void CatalyticConverter::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int CatalyticConverter::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void CatalyticConverter::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int CatalyticConverter::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void CatalyticConverter::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40,20);
}

/////////////////////////////////////////////////////////////////////////////
void CatalyticConverter::DrawIcon(wxDC* dc)
{
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxCYAN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* CatalyticConverter::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new CatalyticConverter_UI_Dialog (parent, -1,
     &effect,
     &site_den,
     &hydDiameter,
     &length,
     &conversion,
     &velocity,
     &case_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString CatalyticConverter::GetName()
{
  wxString result="REI_LarryRuth_CatalyticConverter"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString CatalyticConverter::GetDesc()
{
  wxString result= "Catalytic Converter Module by REI"; //your description

  return result;
}


