
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "Condenser.h"
#include "Condenser_UI.h"

IMPLEMENT_DYNAMIC_CLASS(Condenser, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
Condenser
::Condenser()
{
  RegistVar("tube_id", &tube_id);
  RegistVar("tube_od", &tube_od);
  RegistVar("tube_length", &tube_length);
  RegistVar("int_press_drop", &int_press_drop);
  RegistVar("ext_press_drop", &ext_press_drop);
  RegistVar("num_tubeH", &num_tubeH);
  RegistVar("num_tubeV", &num_tubeV);

  num_tubeH = 142;
  num_tubeV = 33;
  tube_id = 0.0381;
  tube_od = 0.0508;
  tube_length = 11.948;
  int_press_drop = 0;
  ext_press_drop = 0;
  
  wxString icon_file="Icons/condenser.gif";
  wxImage my_img(icon_file, wxBITMAP_TYPE_GIF);
  icon_w = my_img.GetWidth();
  icon_h = my_img.GetHeight();
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  n_pts = 4;

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);
}



/////////////////////////////////////////////////////////////////////////////
Condenser
::~Condenser()
{

}

/////////////////////////////////////////////////////////////////////////////
double Condenser::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int Condenser::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void Condenser::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int Condenser::GetNumIports()
{
  int result=2;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Condenser::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*41/80, icon_h*8/75);
  iports[1]=wxPoint(icon_w*12/80,icon_h*40/75);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int Condenser::GetNumOports()
{
  int result=2;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void Condenser::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*41/80,icon_h*70/75);
  oports[1]=wxPoint(icon_w*70/80,icon_h*40/75);
}

/////////////////////////////////////////////////////////////////////////////
void Condenser::DrawIcon(wxDC* dc)
{
  //Your implementation
  dc->DrawBitmap(*my_icon,pos.x, pos.y);
  /*
  wxBrush old_brush=dc->GetBrush();
  dc->SetBrush(*wxGREEN_BRUSH);
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  dc->SetBrush(old_brush);*/
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* Condenser::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new Condenser_UI_Dialog (parent, -1,
     &tube_id,
     &tube_od,
     &tube_length,
     &int_press_drop,
     &ext_press_drop,
     &num_tubeH,
     &num_tubeV);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString Condenser::GetName()
{
  wxString result="REI_Components_Condenser"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString Condenser::GetDesc()
{
  wxString result="Condenser Module by REI"; //your description

  return result;
}

wxString Condenser::GetHelp()
{
  wxString result="Framework/doc/modules/Condenser.html"; //your description

  return result;
}

