#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)


#include "Stack.h"

IMPLEMENT_DYNAMIC_CLASS(STACK, REI_Plugin)

STACK::STACK()
{
  RegistVar("x", &x);
  RegistVar("y", &y);
  RegistVar("z", &z);
  
  n_pts=4;
  //  wxString stack_icon_file="stack.jpg";
  // wxImage my_img(stack_icon_file, wxBITMAP_TYPE_JPEG);
  // icon_w = my_img.GetWidth() / 5;
  // icon_h = my_img.GetHeight() / 5;
  // my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  //  poly[0]=wxPoint(0,0);
  //poly[1]=wxPoint(icon_w,0);
  // poly[2]=wxPoint(icon_w,icon_h);
  //poly[3]=wxPoint(0,icon_h);
  
  poly[0]=wxPoint(10,0);
  poly[1]=wxPoint(20,0);
  poly[2]=wxPoint(30,100);
  poly[3]=wxPoint(0,100);
}

wxString STACK::GetName()
{
  return _T("CMU_IECM_STACK");
}

wxString STACK::GetDesc()
{
  return _T("This is a Stack");
}

UIDialog* STACK::UI(wxWindow* parent)
{
  return NULL;
}

void STACK::DrawIcon(wxDC* dc)
{
  //  dc->DrawBitmap(*my_icon,pos.x, pos.y);
  wxBrush old_brush=dc->GetBrush();
  wxCoord xoff = pos.x;
  wxCoord yoff = pos.y;
  wxColour* color;
  
  color = wxTheColourDatabase->FindColour("DARK GREEN");
  wxBrush mybrush((*color), wxSOLID);
  dc->SetBrush(mybrush);
  
  dc->DrawPolygon(n_pts, poly, xoff, yoff);
  
  dc->SetBrush(old_brush);
}

void STACK::GetIPorts(POLY &iports)
{
  // iports[0]=wxPoint(icon_w*6/30, icon_h*58/64);
  iports[0]= wxPoint(5,50);
}

void STACK::GetOPorts(POLY &oports)
{
  //oports[0] = wxPoint(icon_w*24/30, icon_h*58/64);
  
  oports[0] =  wxPoint(25,50);
}
