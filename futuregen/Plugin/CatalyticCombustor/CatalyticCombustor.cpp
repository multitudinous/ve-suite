
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "CatalyticCombustor.h"
#include "CatalyticCombustor_UI.h"

IMPLEMENT_DYNAMIC_CLASS(CatalyticCombustor, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
CatalyticCombustor
::CatalyticCombustor()
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

  wxString icon_file="Icons/cat_comb.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);

}



/////////////////////////////////////////////////////////////////////////////
CatalyticCombustor
::~CatalyticCombustor()
{

}

/////////////////////////////////////////////////////////////////////////////
double CatalyticCombustor::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int CatalyticCombustor::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void CatalyticCombustor::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int CatalyticCombustor::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void CatalyticCombustor::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0, icon_h/2);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int CatalyticCombustor::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void CatalyticCombustor::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w, icon_h/2);
}

/////////////////////////////////////////////////////////////////////////////
void CatalyticCombustor::DrawIcon(wxDC* dc)
{
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
	/*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxCYAN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  */
  //Your implementation
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* CatalyticCombustor::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new CatalyticCombustor_UI_Dialog (parent, -1,
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
wxString CatalyticCombustor::GetName()
{
  wxString result="REI_LarryRuth_CatalyticCombustor"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString CatalyticCombustor::GetDesc()
{
  wxString result= "Catalytic Combustor Module by REI"; //your description

  return result;
}


