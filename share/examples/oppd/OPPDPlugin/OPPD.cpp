#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#pragma warning(disable : 4251)

#include "OPPD.h"
#include "OPPD_UI.h"
#include <iostream>
//#include "string_ops.h"

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
  RegistVar("fv1thicktime", &fv1thicktime);
  RegistVar("fv1thicktemp", &fv1thicktemp);
  RegistVar("fv2thicktime", &fv2thicktime);
  RegistVar("fv2thicktemp", &fv2thicktemp);
  RegistVar("nvthicktime", &nvthicktime);
  RegistVar("nvthicktemp", &nvthicktemp);
  RegistVar("killexcel", &killexcel);
  RegistVar("tsec", &tsec);
  RegistVar("tmin", &tmin);
  RegistVar("hrrkw", &hrrkw);
  RegistVar("hrrbtu", &hrrbtu);
  RegistVar("detsprinktime", &detsprinktime);
  RegistVar("detsmtime", &detsmtime);
  RegistVar("detfthtime", &detfthtime);
  RegistVar("flwallinehgt", &flwallinehgt);
  RegistVar("flcornerhgt", &flcornerhgt);
  RegistVar("flwallhgt", &flwallhgt);
  RegistVar("hrrhrr", &hrrhrr);
  RegistVar("hrrburndur", &hrrburndur);
  RegistVar("hrrhgthesk", &hrrhgthesk);
  RegistVar("hrrhgtthom", &hrrhgtthom);
  RegistVar("pltemp", &pltemp);
  RegistVar("tcltemp", &tcltemp);
  RegistVar("visdist", &visdist);
  RegistVar("fv1thintemp", &fv1thintemp);
  RegistVar("fv2thintemp", &fv2thintemp);
  RegistVar("nvthintemp", &nvthintemp);

  wxString icon_file="Icons/bonfire.gif";
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
     &detectpardbls,
	 &fv1thicktime,
	 &fv1thicktemp,
	 &fv2thicktime,
	 &fv2thicktemp,
	 &nvthicktime,
	 &nvthicktemp,
	 &tsec,
     &tmin,
     &hrrkw,
     &hrrbtu,
     &detsprinktime,
     &detsmtime,
     &detfthtime,
     &flwallinehgt,
     &flcornerhgt,
     &flwallhgt,
     &hrrhrr,
     &hrrburndur,
	 &hrrhgthesk,
     &hrrhgtthom,
     &pltemp,
     &tcltemp,
     &visdist,
	 &fv1thintemp,
     &fv2thintemp,
     &nvthintemp);
      
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

UIDialog* OPPD::Result(wxWindow* parent)
{
	if ( v_value.empty() )
      return NULL;
	for ( unsigned int i = 0; i < v_desc.size(); i++ )
    {
      if( !v_desc[ i ].Cmp( "tsec" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_tsec->SetValue(v_value[ i ]);
         
      }
	  else if( !v_desc[ i ].Cmp( "tmin" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_tmin->SetValue(v_value[ i ]);
      }
	  else if( !v_desc[ i ].Cmp( "hrrkw" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_hrrkw->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "hrrbtu" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_hrrbtu->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "detsprinktime" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_detsprinktime->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "detsmtime" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_detsmtime->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "detfthtime" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_detfthtime->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "flwallinehgt" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_flwallinehgt->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "flcornerhgt" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_flcornerhgt->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "flwallhgt" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_flwallhgt->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "hrrhrr" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_hrrhrr->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "hrrburndur" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_hrrburndur->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "hrrhgthesk" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_hrrhgthesk->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "hrrhgtthom" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_hrrhgtthom->SetValue(v_value[ i ]);

      }
	  else if( !v_desc[ i ].Cmp( "pltemp" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_pltemp->SetValue(v_value[ i ]);

	  }
	  else if( !v_desc[ i ].Cmp( "tcltemp" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_tcltemp->SetValue(v_value[ i ]);

      }
	   else if( !v_desc[ i ].Cmp( "visdist" ) )
      {
         //const string var( v_value[ i ].c_str() );
		 //std::cout << var << std::endl;
		 ((OPPD_UI_Dialog*)dlg)->_tabs->_outputsPage->_visdist->SetValue(v_value[ i ]);

      }
      

   }

	return NULL;
}

