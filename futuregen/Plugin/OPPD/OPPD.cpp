
#pragma warning(disable:4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "OPPD.h"
#include "OPPD_UI.h"

IMPLEMENT_DYNAMIC_CLASS(OPPD, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
OPPD
::OPPD()
{
  RegistVar("intlinthickdbl", &intlinthickdbl);
  RegistVar("massfuelburndbl", &massfuelburndbl);
  RegistVar("solidfuelareadbl", &solidfuelareadbl);
  RegistVar("evalabvfiredbl", &evalabvfiredbl);
  RegistVar("cblburnareadbl", &cblburnareadbl);
  RegistVar("tempmethod", &tempmethod);
  RegistVar("tempcalcmethod", &tempcalcmethod);
  RegistVar("detectortype", &detectortype);
  RegistVar("flametype", &flametype);
  RegistVar("detacttemp", &detacttemp);
  RegistVar("matselindex", &matselindex);
  RegistVar("fuelselindex", &fuelselindex);
  RegistVar("vismatselindex", &vismatselindex);
  RegistVar("durmatselindex", &durmatselindex);
  RegistVar("vispropselindex", &vispropselindex);
  RegistVar("viscombselindex", &viscombselindex);
  RegistVar("detrtiselindex", &detrtiselindex);
  RegistVar("dettempratselindex", &dettempratselindex);
  RegistVar("detspaceselindex", &detspaceselindex);
  RegistVar("cableselindex", &cableselindex);
  RegistVar("fuelpardbls", &fuelpardbls);
  RegistVar("compardbls", &compardbls);
  RegistVar("ambpardbls", &ambpardbls);
  RegistVar("ventpardbls", &ventpardbls);
  RegistVar("detectpardbls", &detectpardbls);
  RegistVar("killexcel", &killexcel);

  wxString icon_file="Icons/compressor.gif";
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
OPPD
::~OPPD()
{

}

/////////////////////////////////////////////////////////////////////////////
double OPPD::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int OPPD::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
//void OPPD::GetPoly(POLY &polygon)
//{
//  return ;//polygon;
//}

/////////////////////////////////////////////////////////////////////////////
int OPPD::GetNumIports()
{
  int result=1;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void OPPD::GetIPorts(POLY &iports)
{
  iports[0]=wxPoint(icon_w*10/52, icon_h*26/98);
  return;
}

/////////////////////////////////////////////////////////////////////////////
int OPPD::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void OPPD::GetOPorts(POLY &oports)
{
  oports[0]=wxPoint(icon_w*43/52,icon_h*74/98);   
}

/////////////////////////////////////////////////////////////////////////////
void OPPD::DrawIcon(wxDC* dc)
{
  //Your implementation
	dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* OPPD::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new OPPD_UI_Dialog(parent, -1,
     &intlinthickdbl,
     &massfuelburndbl,
     &solidfuelareadbl,
     &evalabvfiredbl,
     &cblburnareadbl,
     &tempmethod,
     &tempcalcmethod,
     &detectortype,
     &flametype,
     &detacttemp,
     &matselindex,
     &fuelselindex,
     &vismatselindex,
     &durmatselindex,
     &vispropselindex,
     &viscombselindex,
     &detrtiselindex,
     &dettempratselindex,
     &detspaceselindex,
     &cableselindex,
	 &killexcel,
     &fuelpardbls,
     &compardbls,
     &ambpardbls,
     &ventpardbls,
     &detectpardbls);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString OPPD::GetName()
{
  wxString result="OPPD_Fire_Model"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString OPPD::GetDesc()
{
  wxString result="Implementation of FIVE Methodology"; //your description

  return result;
}


