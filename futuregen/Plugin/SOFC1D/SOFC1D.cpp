
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "SOFC1D.h"
#include "SOFC1D_UI.h"

IMPLEMENT_DYNAMIC_CLASS(SOFC1D, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
SOFC1D
::SOFC1D()
{
  RegistVar("a_height", &a_height);
  RegistVar("a_width", &a_width);
  RegistVar("a_space", &a_space);
  RegistVar("a_ecd", &a_ecd);
  RegistVar("a_tcoeff", &a_tcoeff);
  RegistVar("a_thick", &a_thick);
  RegistVar("a_presdrop", &a_presdrop);
  RegistVar("c_height", &c_height);
  RegistVar("c_width", &c_width);
  RegistVar("c_space", &c_space);
  RegistVar("c_ecdb", &c_ecdb);
  RegistVar("c_ecdm", &c_ecdm);
  RegistVar("c_tcoeffb", &c_tcoeffb);
  RegistVar("c_tcoeffm", &c_tcoeffm);
  RegistVar("c_thick", &c_thick);
  RegistVar("c_presdrop", &c_presdrop);
  RegistVar("s_thick", &s_thick);
  RegistVar("s_heatcap", &s_heatcap);
  RegistVar("s_density", &s_density);
  RegistVar("e_thick", &e_thick);
  RegistVar("e_preexp", &e_preexp);
  RegistVar("e_exp", &e_exp);
  RegistVar("f_preexp", &f_preexp);
  RegistVar("f_exp", &f_exp);
  RegistVar("a_preexp", &a_preexp);
  RegistVar("a_exp", &a_exp);
  RegistVar("i_preexp", &i_preexp);
  RegistVar("i_exp", &i_exp);
  RegistVar("l_heatcap", &l_heatcap);
  RegistVar("l_density", &l_density);
  RegistVar("l_length", &l_length);
  RegistVar("l_width", &l_width);
  RegistVar("stop_time", &stop_time);
  RegistVar("loadres", &loadres);
  RegistVar("work_dir", &work_dir);
  RegistVar("l_numCells", &l_numCells);
  RegistVar("ax_nodes", &ax_nodes);

  work_dir = "";

  a_height = 0.001;
  a_width = 0.001;
  a_space = 0.001;
  a_ecd = 5.0;
  a_tcoeff = 1000000.0;
  a_thick = 0.00005;
  a_presdrop = 3250.0;
  
  c_height = 0.003;
  c_width = 0.001;
  c_space = 0.001;
  c_ecdb = -13595.0;
  c_ecdm = 15.0;
  c_tcoeffb = 0.5;
  c_tcoeffm = 0.0;
  c_thick = 0.00005;
  c_presdrop = 431.0;

  s_thick = 0.002;
  s_heatcap = 400.0;
  s_density = 8000.0;

  e_thick = 0.00018;
  e_preexp = 0.002838;
  e_exp = 10300.0;

  f_preexp = 0.0014;
  f_exp = 0.0;
  a_preexp = 0.0186;
  a_exp = 0.0;

  i_preexp = 0.5;
  i_exp = 0.0;
  
  l_numCells =1000;
  l_heatcap = 800.0;
  l_density = 1500.0;
  l_length = 1.0;
  l_width = 1.0;

  stop_time = 1000.0;
  ax_nodes = 5;
  loadres = 0.006;

  n_pts = 3;
}



/////////////////////////////////////////////////////////////////////////////
SOFC1D
::~SOFC1D()
{

}

/////////////////////////////////////////////////////////////////////////////
double SOFC1D::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int SOFC1D::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void SOFC1D::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int SOFC1D::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SOFC1D::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0, 20);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int SOFC1D::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SOFC1D::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40, 20);
}

/////////////////////////////////////////////////////////////////////////////
void SOFC1D::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREY_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* SOFC1D::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new SOFC1D_UI_Dialog (parent, -1,
     &a_height,
     &a_width,
     &a_space,
     &a_ecd,
     &a_tcoeff,
     &a_thick,
     &a_presdrop,
     &c_height,
     &c_width,
     &c_space,
     &c_ecdb,
     &c_ecdm,
     &c_tcoeffb,
     &c_tcoeffm,
     &c_thick,
     &c_presdrop,
     &s_thick,
     &s_heatcap,
     &s_density,
     &e_thick,
     &e_preexp,
     &e_exp,
     &f_preexp,
     &f_exp,
     &a_preexp,
     &a_exp,
     &i_preexp,
     &i_exp,
     &l_heatcap,
     &l_density,
     &l_length,
     &l_width,
     &stop_time,
     &loadres,
     &work_dir,
     &l_numCells,
     &ax_nodes);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString SOFC1D::GetName()
{
  wxString result="REI_LarryRuth_SOFC1D"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString SOFC1D::GetDesc()
{
  wxString result="SOFC 1D Module by REI"; //your description

  return result;
}


