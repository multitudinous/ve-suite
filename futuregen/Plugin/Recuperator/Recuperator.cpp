
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Recuperator.h"
#include "Recuperator_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Recuperator, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Recuperator
::Recuperator()
{
  RegistVar("Sl", &Sl);
  RegistVar("St", &St);
  RegistVar("tube_id", &tube_id);
  RegistVar("tube_od", &tube_od);
  RegistVar("tube_length", &tube_length);
  RegistVar("int_press_drop", &int_press_drop);
  RegistVar("ext_press_drop", &ext_press_drop);
  RegistVar("fin_effect", &fin_effect);
  RegistVar("arrangement", &arrangement);
  RegistVar("tube_config", &tube_config);
  RegistVar("num_tubeL", &num_tubeL);
  RegistVar("num_tubeX", &num_tubeX);
  RegistVar("use_fins", &use_fins);
  
  arrangement = "Shell & Tube";
  tube_config = "Inline";
  num_tubeL = 142;
  num_tubeX = 33;
  Sl = 0.127;
  St = 0.0826;
  tube_id = 0.0381;
  tube_od = 0.0508;
  tube_length = 11.948;
  int_press_drop = 0;
  ext_press_drop = 0;
  use_fins = 1;
  fin_effect = 2.0;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(40,15);
  poly[2]=wxPoint(40,25);
  poly[3]=wxPoint(0, 40);
  n_pts=4;
}



/////////////////////////////////////////////////////////////////////////////
Recuperator
::~Recuperator()
{

}

/////////////////////////////////////////////////////////////////////////////
double Recuperator::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Recuperator::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Recuperator::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Recuperator::GetNumIports()
{
  int result=2;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Recuperator::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(0,15);
  iports[1]=wxPoint(0,25);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Recuperator::GetNumOports()
{
  int result=2;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Recuperator::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(40,15);
  oports[1]=wxPoint(40,25);
}

/////////////////////////////////////////////////////////////////////////////
void Recuperator::DrawIcon(wxDC* dc)
{
  //Your implementation
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxBLUE_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Recuperator::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Recuperator_UI_Dialog (parent, -1,
     &Sl,
     &St,
     &tube_id,
     &tube_od,
     &tube_length,
     &int_press_drop,
     &ext_press_drop,
     &fin_effect,
     &arrangement,
     &tube_config,
     &num_tubeL,
     &num_tubeX,
     &use_fins);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Recuperator::GetName()
{
  wxString result="REI_LarryRuth_Recuperator"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Recuperator::GetDesc()
{
  wxString result="Recuperator Module by REI"; //your description

  return result;
}


