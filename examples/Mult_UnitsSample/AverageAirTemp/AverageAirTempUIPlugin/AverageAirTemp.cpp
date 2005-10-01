#include "AverageAirTemp.h"
#include "AverageAirTemp_UI_Dialog.h"

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
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void AverageAirTemp::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void AverageAirTemp::DrawIcon(wxDC* dc)
{
  //Your implementation
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
     &intakelength);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString AverageAirTemp::GetName()
{
  wxString result="NEW_MOD"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString AverageAirTemp::GetDesc()
{
  wxString result="None"; //your description

  return result;
}


