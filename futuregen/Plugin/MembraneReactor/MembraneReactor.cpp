
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "MembraneReactor.h"
#include "MembraneReactor_UI.h"

IMPLEMENT_DYNAMIC_CLASS(MembraneReactor, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
MembraneReactor
::MembraneReactor()
{
  RegistVar("memb_diameter", &memb_diameter);
  RegistVar("Pd_thickness", &Pd_thickness);
  RegistVar("L_rxr", &L_rxr);
  RegistVar("CO_conv_want", &CO_conv_want);
  RegistVar("shell_diameter", &shell_diameter);
  RegistVar("mr_inlet_temp", &mr_inlet_temp);
  RegistVar("H2O_CO", &H2O_CO);
  RegistVar("case_type", &case_type);
  RegistVar("n_modules", &n_modules);
  RegistVar("f_pre_mr", &f_pre_mr);
  RegistVar("f_H2O_CO", &f_H2O_CO);

  case_type = 0;
  memb_diameter = 0.1;
  Pd_thickness = 10.0E-6;
  L_rxr = 1.5;
  CO_conv_want = 0.90;
  n_modules = 1200;
  shell_diameter = 0.15;
  f_pre_mr = 1;
  mr_inlet_temp = 565.0;
  f_H2O_CO = 1;
  H2O_CO = 1.0;
 
  wxString icon_file="Icons/membrane_reactor.gif";
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
MembraneReactor
::~MembraneReactor()
{

}

/////////////////////////////////////////////////////////////////////////////
double MembraneReactor::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int MembraneReactor::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void MembraneReactor::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int MembraneReactor::GetNumIports()
{
  int result=2;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void MembraneReactor::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*19/90,icon_h*15/75);
  iports[1]=wxPoint(icon_w*19/90,icon_h*58/75);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int MembraneReactor::GetNumOports()
{
  int result=2;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void MembraneReactor::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*70/90,icon_h*16/75); 
  oports[1]=wxPoint(icon_w*70/90,icon_h*58/75);
}

/////////////////////////////////////////////////////////////////////////////
void MembraneReactor::DrawIcon(wxDC* dc)
{
  /*wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxLIGHT_GREY_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
  //Your implementation*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* MembraneReactor::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new MembraneReactor_UI_Dialog (parent, -1,
     &memb_diameter,
     &Pd_thickness,
     &L_rxr,
     &CO_conv_want,
     &shell_diameter,
     &mr_inlet_temp,
     &H2O_CO,
     &case_type,
     &n_modules,
     &f_pre_mr,
     &f_H2O_CO);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString MembraneReactor::GetName()
{
  wxString result="REI_Components_MembraneReactor"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString MembraneReactor::GetDesc()
{
  wxString result="Memebrane Reactor Module by REI"; //your description

  return result;
}

wxString MembraneReactor::GetHelp()
{
  wxString result="Framework/doc/modules/Membrane_Reactor.html"; //your description

  return result;
}

