#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "REI_Gasi.h"
#include "REI_Gasi_UI.h"

IMPLEMENT_DYNAMIC_CLASS(REI_Gasi, REI_Plugin)

REI_Gasi::REI_Gasi()
{
  RegistVar("x",&x);
  RegistVar("y", &y);
  RegistVar("z", &z);
  
  if (poly!=NULL)
    delete poly;

  wxString gasi_icon_file="REIgassifier.jpg";
  wxImage my_img(gasi_icon_file, wxBITMAP_TYPE_JPEG);
  icon_w = my_img.GetWidth() / 5;
  icon_h = my_img.GetHeight() / 5;
  my_icon=new wxBitmap(my_img.Scale(icon_w, icon_h));

  poly = new wxPoint[4];
  n_pts = 4;

  //  my_icon->SetWidth(icon_w);
  //my_icon->SetHeight(icon_h);

  poly[0]=wxPoint(0,0);
  poly[1]=wxPoint(icon_w,0);
  poly[2]=wxPoint(icon_w,icon_h);
  poly[3]=wxPoint(0,icon_h);

  //  poly = new wxPoint[16];
  // n_pts = 16;
  //poly[0]=wxPoint(0,8);
  //poly[1]=wxPoint(4,0);
  //poly[2]=wxPoint(32,0);
  //poly[3]=wxPoint(36,8);
  //poly[4]=wxPoint(36,84);
  //poly[5]=wxPoint(28,92);
  //poly[6]=wxPoint(28,100);
  //poly[7]=wxPoint(36,108);
  // poly[8]=wxPoint(36,128);
  //poly[9]=wxPoint(32,136);
  // poly[10]=wxPoint(4,136);
  //poly[11]=wxPoint(0,128);
  //poly[12]=wxPoint(0,108);
  //poly[13]=wxPoint(8,100);
  //poly[14]=wxPoint(8,92);
  //poly[15]=wxPoint(0,84);

    
}

wxString REI_Gasi::GetName()
{
  return _T("REI_CFD_Gasifier");
}

wxString REI_Gasi::GetDesc()
{
  return _T("This will be a real module for the REI_Gasi.\n Now is just empty for testing name binding.\n");
}

//int REI_Gasi::GetNumPoly()
//{
//	return n_pts;
//}


void REI_Gasi::DrawIcon(wxDC* dc)
{
   dc->DrawBitmap(*my_icon,pos.x, pos.y);
   
  // wxBrush old_brush=dc->GetBrush();
  //wxColour* color;
  //color = wxTheColourDatabase->FindColour("YELLOW");
  //wxBrush mybrush((*color), wxSOLID);
  //dc->SetBrush(mybrush);

  //wxCoord xoff = pos.x;
  //wxCoord yoff = pos.y;
  //dc->DrawPolygon(n_pts, poly, xoff, yoff);
  //dc->SetBrush(old_brush);
}

UIDialog* REI_Gasi::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
 
  dlg = new REI_Gasi_UI(parent, -1);

  return dlg;
}

void REI_Gasi::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*2/5, icon_h/10);
}

void REI_Gasi::GetOPorts(POLY &oports)
{
  //oports[0] = wxPoint(icon_w*9/20,icon_h*19/20);
  //  oports[0] = wxPoint(36, 115);
  oports[0] = wxPoint(icon_w*9/10-2,icon_h/2);
}
