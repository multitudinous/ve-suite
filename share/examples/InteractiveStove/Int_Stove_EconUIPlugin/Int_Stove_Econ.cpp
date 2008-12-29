#include "Int_Stove_Econ.h"
#include "Int_Stove_Econ_UI_Dialog.h"

IMPLEMENT_DYNAMIC_CLASS(Int_Stove_Econ, ves::conductor::UIPluginBase)

/////////////////////////////////////////////////////////////////////////////
Int_Stove_Econ
::Int_Stove_Econ()
{
  RegistVar("cost_array", &cost_array);
  RegistVar("closesheets", &closesheets);

  wxString icon_file=_T("Icons/dollar.gif");
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
    SetImage( my_img );

    mPluginName = _T("IntStoveEcon");
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
int Int_Stove_Econ::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Int_Stove_Econ::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(GetIconImage()->GetWidth()*3/43,GetIconImage()->GetHeight()*21/41);
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
  wxString result=_T("IntStoveEcon"); //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Int_Stove_Econ::GetDesc()
{
  wxString result=_T("IntStoveEcon"); //your description

  return result;
}


