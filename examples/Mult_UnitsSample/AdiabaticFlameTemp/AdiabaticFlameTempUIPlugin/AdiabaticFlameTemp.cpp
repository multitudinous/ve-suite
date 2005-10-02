#include "AdiabaticFlameTemp.h"
#include "AdiabaticFlameTemp_UI_Dialog.h"

IMPLEMENT_DYNAMIC_CLASS(AdiabaticFlameTemp, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
AdiabaticFlameTemp
::AdiabaticFlameTemp()
{
   RegistVar("perc_theor_error", &perc_theor_error);
   RegistVar("closesheets", &closesheets);

   perc_theor_error = 0;

   wxString icon_file="Icons/Adiab_Flame_Temp_Mod.GIF";
   wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
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
AdiabaticFlameTemp
::~AdiabaticFlameTemp()
{

}

/////////////////////////////////////////////////////////////////////////////
double AdiabaticFlameTemp::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int AdiabaticFlameTemp::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int AdiabaticFlameTemp::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AdiabaticFlameTemp::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*3/43,icon_h*21/41);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int AdiabaticFlameTemp::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AdiabaticFlameTemp::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void AdiabaticFlameTemp::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* AdiabaticFlameTemp::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new AdiabaticFlameTemp_UI_Dialog(parent, -1,
     &perc_theor_error,
     &closesheets);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString AdiabaticFlameTemp::GetName()
{
  wxString result="SampleApps_MultipleUnit_AdiabFlameTemp"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString AdiabaticFlameTemp::GetDesc()
{
  wxString result="Adiabatic Flame Temp Module"; //your description

  return result;
}


