
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "GasSplitter.h"
#include "GasSplitter_UI.h"

IMPLEMENT_DYNAMIC_CLASS(GasSplitter, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GasSplitter
::GasSplitter()
{
  RegistVar("percent_port1", &percent_port1);
  RegistVar("percent_port2", &percent_port2);
  RegistVar("percent_port3", &percent_port3);
  RegistVar("percent_port4", &percent_port4);
  
  percent_port1 = 50.0;
  percent_port2 = 50.0;
  percent_port3 = 0.0;
  percent_port4 = 0.0;

   wxString icon_file="Icons/gas_splitter.gif";
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
GasSplitter
::~GasSplitter()
{

}

/////////////////////////////////////////////////////////////////////////////
double GasSplitter::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GasSplitter::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void GasSplitter::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int GasSplitter::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasSplitter::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*6/60, icon_h*20/45);

  return;
}

/////////////////////////////////////////////////////////////////////////////
int GasSplitter::GetNumOports()
{
  int result=4;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasSplitter::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*52/60, icon_h*11/45);
  oports[1]=wxPoint(icon_w*52/60, icon_h*18/45);
  oports[2]=wxPoint(icon_w*52/60, icon_h*26/45);
  oports[3]=wxPoint(icon_w*52/60, icon_h*33/45);
}

/////////////////////////////////////////////////////////////////////////////
void GasSplitter::DrawIcon(wxDC* dc)
{
  //Your implementation
 /* wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLUE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GasSplitter::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new GasSplitter_UI_Dialog (parent, -1,
     &percent_port1,
     &percent_port2,
     &percent_port3,
     &percent_port4);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasSplitter::GetName()
{
  wxString result="REI_Gas_GasSplitter"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasSplitter::GetDesc()
{
  wxString result="Gas Splitter module by REI"; //your description

  return result;
}

wxString GasSplitter::GetHelp()
{
  wxString result="Framework/doc/modules/GasSplitter.html"; //your description

  return result;
}

