
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Exterior.h"
#include "Exterior_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Exterior, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Exterior
::Exterior()
{
  RegistVar("screenDiameter", &screenDiameter);
  RegistVar("directionWind", &directionWind);

  wxString icon_file="Icons/deere_combine_exterior.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth()/5;
  icon_h = my_img.GetHeight()/5;
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);

}



/////////////////////////////////////////////////////////////////////////////
Exterior
::~Exterior()
{

}

/////////////////////////////////////////////////////////////////////////////
double Exterior::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Exterior::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}
/*
/////////////////////////////////////////////////////////////////////////////
void Exterior::GetPoly(POLY &polygon)
{
  return ;//polygon;
}
*/
/////////////////////////////////////////////////////////////////////////////
int Exterior::GetNumIports()
{
  int result=0;
  // For cooling 1 input
   result = 0;
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Exterior::GetIPorts(POLY &iports)
{
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Exterior::GetNumOports()
{
  int result=0;
  //Your code
   result = 1;
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Exterior::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*0.55,icon_h*0.42);     
//  oports[1]=wxPoint(icon_w*0.28,icon_h*0.96);     
}

/////////////////////////////////////////////////////////////////////////////
void Exterior::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Exterior::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Exterior_UI_Dialog(parent, -1,
     &screenDiameter,
     &directionWind);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Exterior::GetName()
{
  wxString result="JD_Engine_EXTERIOR"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Exterior::GetDesc()
{
  wxString result="Defining Wind Direction and Rotary Screen Size for Harvester Engine Compartment."; //your description

  return result;
}


