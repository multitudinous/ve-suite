
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Baffle.h"
#include "Baffle_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Baffle, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Baffle
::Baffle()
{
  RegistVar("xLocation", &xLocation);
  RegistVar("yLocation", &yLocation);
  RegistVar("zLocation", &zLocation);
  RegistVar("height", &height);
  RegistVar("width", &width);


  wxString icon_file="Icons/deere_engine_drivetrain.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth()*0.20;
  icon_h = my_img.GetHeight()*0.20;
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);

}



/////////////////////////////////////////////////////////////////////////////
Baffle
::~Baffle()
{

}

/////////////////////////////////////////////////////////////////////////////
double Baffle::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Baffle::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}
/*
/////////////////////////////////////////////////////////////////////////////
void Baffle::GetPoly(POLY &polygon)
{
  return ;//polygon;
}
*/
/////////////////////////////////////////////////////////////////////////////
int Baffle::GetNumIports()
{
  int result=1;
  
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Baffle::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*0.0, icon_h*0.50);
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Baffle::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Baffle::GetOPorts(POLY &oports)
{
  //oports[0]=wxPoint(icon_w*43/52,icon_h*74/98);     
}

/////////////////////////////////////////////////////////////////////////////
void Baffle::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Baffle::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Baffle_UI_Dialog(parent, -1,
     &xLocation,
     &yLocation,
     &zLocation,
     &height,
     &width);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Baffle::GetName()
{
  wxString result="JD_Engine_ENGINE/BAFFLE"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Baffle::GetDesc()
{
  wxString result="Defining location and size of baffle around engine compartment."; //your description

  return result;
}


