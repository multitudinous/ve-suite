
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Gasifier.h"
#include "Gasifier_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Gasifier, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Gasifier
::Gasifier()
{
  RegistVar("steam_temp1", &steam_temp1);
  RegistVar("steam_flrt1", &steam_flrt1);
  RegistVar("slurry_temp1", &slurry_temp1);
  RegistVar("slurry_flrt1", &slurry_flrt1);
  RegistVar("coal_percent1", &coal_percent1);
  RegistVar("char_percent1", &char_percent1);
  RegistVar("steam_temp2", &steam_temp2);
  RegistVar("steam_flrt2", &steam_flrt2);
  RegistVar("slurry_temp2", &slurry_temp2);
  RegistVar("slurry_flrt2", &slurry_flrt2);
  RegistVar("coal_percent2", &coal_percent2);
  RegistVar("char_percent2", &char_percent2);
  RegistVar("steam_temp3", &steam_temp3);
  RegistVar("steam_flrt3", &steam_flrt3);
  RegistVar("slurry_temp3", &slurry_temp3);
  RegistVar("slurry_flrt3", &slurry_flrt3);
  RegistVar("coal_percent3", &coal_percent3);
  RegistVar("char_percent3", &char_percent3);
  RegistVar("size_50", &size_50);
  RegistVar("size_200", &size_200);
  RegistVar("pres_drop", &pres_drop);
  RegistVar("coal_type", &coal_type);
  RegistVar("stage", &stage);
  
  stage = 1;
  steam_temp1 = steam_temp2 = steam_temp3 = 0.0;
  steam_flrt1 = steam_flrt2 = steam_flrt3 = 0.0;
  coal_percent1 = coal_percent2 = coal_percent3 = 74.26;
  char_percent1 = char_percent2 = char_percent3 = 0.0;
  slurry_temp1 = slurry_temp2 = slurry_temp3 = 422.0;
  slurry_flrt1 = slurry_flrt2 = slurry_flrt3= 43.46;
  
  pres_drop = 0.0;

  size_50 = 99.9;
  size_200 = 90.0;
  wxString icon_file="Icons/gasifier2.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  coal_type = "Illinois_#6";

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
}



/////////////////////////////////////////////////////////////////////////////
Gasifier
::~Gasifier()
{

}

/////////////////////////////////////////////////////////////////////////////
double Gasifier::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Gasifier::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Gasifier::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Gasifier::GetNumIports()
{
  int result=3;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*26/63, icon_h*86/123);
  iports[1]=wxPoint(icon_w*20/63, icon_h*105/123);
  iports[2]=wxPoint(icon_w*36/63, icon_h*118/123);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Gasifier::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*37/63, icon_h*20/123);
}

/////////////////////////////////////////////////////////////////////////////
void Gasifier::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Gasifier::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Gasifier_UI_Dialog(parent, -1,
     &steam_temp1,
     &steam_flrt1,
     &slurry_temp1,
     &slurry_flrt1,
     &coal_percent1,
     &char_percent1,
     &steam_temp2,
     &steam_flrt2,
     &slurry_temp2,
     &slurry_flrt2,
     &coal_percent2,
     &char_percent2,
     &steam_temp3,
     &steam_flrt3,
     &slurry_temp3,
     &slurry_flrt3,
     &coal_percent3,
     &char_percent3,
     &size_50,
     &size_200,
     &pres_drop,
     &coal_type,
     &stage);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Gasifier::GetName()
{
  wxString result="REI_Components_Gasifier"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Gasifier::GetDesc()
{
  wxString result="Gasifier Module by REI"; //your description

  return result;
}

/////////////////////////////////////////////////////////////////////////////
bool Gasifier::Has3Ddata()
{
  return true;
}
