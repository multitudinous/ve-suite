
#ifdef WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)
#endif

#include "SampleMFC_Gauges.h"
#include "SampleMFC_Gauges_UI_Dialog.h"

IMPLEMENT_DYNAMIC_CLASS(SampleMFC_Gauges, REI_Plugin)

/////////////////////////////////////////////////////////////////////////////
SampleMFC_Gauges
::SampleMFC_Gauges()
{
  RegistVar("dbl1", &dbl1);
  RegistVar("dbl2", &dbl2);
  RegistVar("int1", &int1);
	RegistVar("int2", &int2);
  RegistVar("dbllist", &dbllist);

  wxString icon_file="Icons/samplemfcgauge.GIF";
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
SampleMFC_Gauges
::~SampleMFC_Gauges()
{

}

/////////////////////////////////////////////////////////////////////////////
double SampleMFC_Gauges::GetVersion()
{
  double result=1.0;

  //Your code

  return result;
}

/////////////////////////////////////////////////////////////////////////////
int SampleMFC_Gauges::GetNumPoly()
{
  int result=0;
  //Your code
  return n_pts;
}

/////////////////////////////////////////////////////////////////////////////
int SampleMFC_Gauges::GetNumIports()
{
  int result=0;

  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SampleMFC_Gauges::GetIPorts(POLY &iports)
{
  
  return;
}

/////////////////////////////////////////////////////////////////////////////
int SampleMFC_Gauges::GetNumOports()
{
  int result=0;
  //Your code
  return result;
}

/////////////////////////////////////////////////////////////////////////////
void SampleMFC_Gauges::GetOPorts(POLY &oports)
{
    
}

/////////////////////////////////////////////////////////////////////////////
void SampleMFC_Gauges::DrawIcon(wxDC* dc)
{
  //Your implementation
		dc->DrawBitmap(*my_icon,pos.x, pos.y);
}

/////////////////////////////////////////////////////////////////////////////
UIDialog* SampleMFC_Gauges::UI(wxWindow* parent)
{
  if (dlg!=NULL)
    return dlg;
  
  dlg = new SampleMFC_Gauges_UI_Dialog(parent, -1,
     &dbl1,
     &dbl2,
     &int1,
		 &int2,
     &dbllist);
      
  return dlg;
}

/////////////////////////////////////////////////////////////////////////////
wxString SampleMFC_Gauges::GetName()
{
  wxString result="Samples_MFCGauges"; //your name
  return result;
}

/////////////////////////////////////////////////////////////////////////////
wxString SampleMFC_Gauges::GetDesc()
{
  wxString result="Sample With MFC and Gauges"; //your description

  return result;
}


