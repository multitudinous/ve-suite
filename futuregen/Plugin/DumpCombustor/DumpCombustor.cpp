
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
  RegistVar("conversion", &conversion);
  RegistVar("volume", &volume);
  RegistVar("fracQloss", &fracQloss);
  RegistVar("press_drop", &press_drop);
  RegistVar("case_type", &case_type);

  conversion = 10.0;
  volume = 0.99;
  fracQloss = 0.0;
  press_drop = 0.0;
  case_type = 0;

  wxString icon_file="Icons/dump_combustor.gif";
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
  int result=0;
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
  iports[0]=wxPoint(icon_w*41/80,icon_h*6/75);
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
  oports[0]=wxPoint(icon_w*41/80,icon_h*69/75);
}

/////////////////////////////////////////////////////////////////////////////
void DumpCombustor::DrawIcon(wxDC* dc)
{
  //Your implementation
  /*wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLUE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* DumpCombustor::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new DumpCombustor_UI_Dialog (parent, -1,
     &conversion,
     &volume,
     &fracQloss,
     &press_drop,
     &case_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString DumpCombustor::GetName()
{
  wxString result="REI_Components_DumpCombustor"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString DumpCombustor::GetDesc()
{
  wxString result="Dump Combustor Module by REI"; //your description

  return result;
}


