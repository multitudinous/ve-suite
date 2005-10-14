#include "AverageAirTemp.h"
#include "AverageAirTemp_UI_Dialog.h"
#include "Aver_Air_Temp_Mod.xpm"

IMPLEMENT_DYNAMIC_CLASS(AverageAirTemp, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
AverageAirTemp
::AverageAirTemp()
{
  RegistVar("intakediam", &intakediam);
  RegistVar("airvel", &airvel);
  RegistVar("intaketemp", &intaketemp);
  RegistVar("airinlettemp", &airinlettemp);
  RegistVar("intakelength", &intakelength);
  RegistVar("closesheets", &closesheets);

  intakediam = 0;
  airvel = 0;
  intaketemp = 0;
  airinlettemp = 0;
  intakelength = 0;

  wxImage my_img( Aver_Air_Temp_Mod_xpm );
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]= wxPoint(0,0);
  poly[1]= wxPoint(icon_w,0);
  poly[2]= wxPoint(icon_w,icon_h);
  poly[3]= wxPoint(0,icon_h);
}



/////////////////////////////////////////////////////////////////////////////
AverageAirTemp
::~AverageAirTemp()
{

}

/////////////////////////////////////////////////////////////////////////////
double AverageAirTemp::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int AverageAirTemp::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int AverageAirTemp::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AverageAirTemp::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int AverageAirTemp::GetNumOports()
{
  int result=1;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AverageAirTemp::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*40/43,icon_h*21/41); 
}

/////////////////////////////////////////////////////////////////////////////
void AverageAirTemp::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* AverageAirTemp::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new AverageAirTemp_UI_Dialog(parent, -1,
     &intakediam,
     &airvel,
     &intaketemp,
     &airinlettemp,
     &intakelength,
     &closesheets);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString AverageAirTemp::GetName()
{
  wxString result="SampleApps_MultipleUnit_AverAirTemp"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString AverageAirTemp::GetDesc()
{
  wxString result="Average Air Temp Module"; //your description

  return result;
}


