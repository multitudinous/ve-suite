
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Cooling.h"
#include "Cooling_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Cooling, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Cooling
::Cooling()
{
  RegistVar("coolantType", &coolantType);
  RegistVar("heightRad", &heightRad);
  RegistVar("widthRad", &widthRad);
  RegistVar("fanPitch", &fanPitch);

  wxString icon_file="Icons/deere_heat_exchanger.gif";
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
Cooling
::~Cooling()
{

}

/////////////////////////////////////////////////////////////////////////////
double Cooling::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Cooling::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}
/*
/////////////////////////////////////////////////////////////////////////////
void Cooling::GetPoly(POLY &polygon)
{
  return ;//polygon;
}
*/
/////////////////////////////////////////////////////////////////////////////
int Cooling::GetNumIports()
{
  int result=0;
// For cooling 1 input
   result = 1;
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Cooling::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*0.20, icon_h*0.50);  
  //iports[0]=wxPoint(icon_w*10/52, icon_h*26/98);  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Cooling::GetNumOports()
{
  int result=0;
  //Your code
   result = 2;
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Cooling::GetOPorts(POLY &oports)
{
  //oports[0]=wxPoint(icon_w*43/52,icon_h*74/98);
  oports[0]=wxPoint(icon_w*0.70,icon_h*0.50);     
  oports[1]=wxPoint(icon_w*0.28,icon_h*0.96);     
        
}

/////////////////////////////////////////////////////////////////////////////
void Cooling::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Cooling::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Cooling_UI_Dialog(parent, -1,
     &coolantType,
     &heightRad,
     &widthRad,
     &fanPitch);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Cooling::GetName()
{
  wxString result="JD_Engine_COOLING"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Cooling::GetDesc()
{
  wxString result="Defining Parameters of Cooling System"; //your description

  return result;
}


