
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "KineticReactor.h"
#include "KineticReactor_UI.h"

IMPLEMENT_DYNAMIC_CLASS(KineticReactor, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
KineticReactor
::KineticReactor()
{
  RegistVar("res_time", &res_time);
  RegistVar("qloss", &qloss);
  RegistVar("quench_rate", &quench_rate);
  RegistVar("work_dir", &work_dir);
  RegistVar("case_type", &case_type);

  res_time = 0.05;
  qloss = 0.0;
  quench_rate = 1000.0;
  work_dir = "";
  case_type = 0;

  wxString icon_file="Icons/kinetic_reactor.gif";
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
KineticReactor
::~KineticReactor()
{

}

/////////////////////////////////////////////////////////////////////////////
double KineticReactor::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int KineticReactor::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void KineticReactor::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int KineticReactor::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void KineticReactor::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*5/60, icon_h*21/45);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int KineticReactor::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void KineticReactor::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*52/60, icon_h*21/45);
}

/////////////////////////////////////////////////////////////////////////////
void KineticReactor::DrawIcon(wxDC* dc)
{
  /*//Your implementation
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  
  dc->DrawPolygon(n_pts, poly, xoff, yoff);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* KineticReactor::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new KineticReactor_UI_Dialog (parent, -1,
     &res_time,
     &qloss,
     &quench_rate,
     &work_dir,
     &case_type);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString KineticReactor::GetName()
{
  wxString result="REI_Gas_KineticReactor"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString KineticReactor::GetDesc()
{
  wxString result="KineticReactor Module by REI"; //your description

  return result;
}



wxString KineticReactor::GetHelp()
{
  wxString result="Framework/doc/modules/Kinetic_Reactor.html"; //your description

  return result;
}