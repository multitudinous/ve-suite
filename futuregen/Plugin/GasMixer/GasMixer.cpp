
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "GasMixer.h"

IMPLEMENT_DYNAMIC_CLASS(GasMixer, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
GasMixer
::GasMixer()
{
 wxString icon_file="Icons/gas_mixer.gif";
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
GasMixer
::~GasMixer()
{

}

/////////////////////////////////////////////////////////////////////////////
double GasMixer::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int GasMixer::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void GasMixer::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int GasMixer::GetNumIports()
{
  int result=4;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasMixer::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*12/100, icon_h*19/75);
  iports[1]=wxPoint(icon_w*12/100, icon_h*31/75);
  iports[2]=wxPoint(icon_w*12/100, icon_h*44/75);
  iports[3]=wxPoint(icon_w*12/100, icon_h*56/75);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int GasMixer::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void GasMixer::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*88/100, icon_h*39/75);
}

/////////////////////////////////////////////////////////////////////////////
void GasMixer::DrawIcon(wxDC* dc)
{
  //Your implementation
/*
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  
  dc->DrawPolygon(n_pts, poly, xoff, yoff);*/
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* GasMixer::UI(wxWindow* parent)
{
  return NULL; // no UI
}

/////////////////////////////////////////////////////////////////////////////
wxString GasMixer::GetName()
{
  wxString result="REI_Gas_GasMixer"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString GasMixer::GetDesc()
{
  wxString result="Gas Mixer Module by REI"; //your description

  return result;
}


