#include "Int_Stove_Econ.h"
#include "Int_Stove_Econ_UI_Dialog.h"

IMPLEMENT_DYNAMIC_CLASS(Int_Stove_Econ, ves::conductor::UIDialog)

/////////////////////////////////////////////////////////////////////////////
Int_Stove_Econ
::Int_Stove_Econ()
{
  RegistVar("cost_array", &cost_array);
  RegistVar("closesheets", &closesheets);

  wxString icon_file="Icons/dollar.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);

    name = "IntStoveEcon";
}

/////////////////////////////////////////////////////////////////////////////
Int_Stove_Econ
::~Int_Stove_Econ()
{

}

/////////////////////////////////////////////////////////////////////////////
double Int_Stove_Econ::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Int_Stove_Econ::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int Int_Stove_Econ::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Int_Stove_Econ::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*3/43,icon_h*21/41);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Int_Stove_Econ::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Int_Stove_Econ::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void Int_Stove_Econ::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
ves::conductor::UIDialog* Int_Stove_Econ::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Int_Stove_Econ_UI_Dialog(parent, -1,
     &cost_array,
	 &closesheets);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Int_Stove_Econ::GetName()
{
  wxString result="IntStoveEcon"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Int_Stove_Econ::GetDesc()
{
  wxString result="IntStoveEcon"; //your description

  return result;
}


