#include "OPPD_UI.h"

IMPLEMENT_DYNAMIC_CLASS(OPPD_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
OPPD_UI_Dialog
::OPPD_UI_Dialog
(wxWindow* parent, int id,
  double* intlinthickdbl,
  double* massfuelburndbl,
  double* solidfuelareadbl,
  double* evalabvfiredbl,
  double* cblburnareadbl,
  long* tempmethod,
  long* tempcalcmethod,
  long* detectortype,
  long* flametype,
  long* detacttemp,
  long* matselindex,
  long* fuelselindex,
  long* vismatselindex,
  long* durmatselindex,
  long* vispropselindex,
  long* viscombselindex,
  long* detrtiselindex,
  long* dettempratselindex,
  long* detspaceselindex,
  long* cableselindex,
  long* killexcel,
  vector<double>* fuelpardbls,
  vector<double>* compardbls,
  vector<double>* ambpardbls,
  vector<double>* ventpardbls,
  vector<double>* detectpardbls,
  vector<double>* fv1thicktime,
  vector<double>* fv1thicktemp,
  vector<double>* fv2thicktime,
  vector<double>* fv2thicktemp,
  vector<double>* nvthicktime,
  vector<double>* nvthicktemp,
  double* tsec,
  double* tmin,
  double* hrrkw,
  double* hrrbtu,
  double* detsprinktime,
  double* detsmtime,
  double* detfthtime,
  double* flwallinehgt,
  double* flcornerhgt,
  double* flwallhgt,
  double* hrrhrr,
  double* hrrburndur,
  double* hrrhgthesk,
  double* hrrhgtthom,
  double* pltemp,
  double* tcltemp,
  double* visdist,
  double* fv1thintemp,
  double* fv2thintemp,
  double* nvthintemp)
: UIDialog((wxWindow *) parent, id, "OPPD"),
  p_intlinthickdbl(intlinthickdbl),
  p_massfuelburndbl(massfuelburndbl),
  p_solidfuelareadbl(solidfuelareadbl),
  p_evalabvfiredbl(evalabvfiredbl),
  p_cblburnareadbl(cblburnareadbl),
  p_tempmethod(tempmethod),
  p_tempcalcmethod(tempcalcmethod),
  p_detectortype(detectortype),
  p_flametype(flametype),
  p_detacttemp(detacttemp),
  p_matselindex(matselindex),
  p_fuelselindex(fuelselindex),
  p_vismatselindex(vismatselindex),
  p_durmatselindex(durmatselindex),
  p_vispropselindex(vispropselindex),
  p_viscombselindex(viscombselindex),
  p_detrtiselindex(detrtiselindex),
  p_dettempratselindex(dettempratselindex),
  p_detspaceselindex(detspaceselindex),
  p_cableselindex(cableselindex),
  p_killexcel(killexcel),
  p_fuelpardbls(fuelpardbls),
  p_compardbls(compardbls),
  p_ambpardbls(ambpardbls),
  p_ventpardbls(ventpardbls),
  p_detectpardbls(detectpardbls),
  p_fv1thicktime(fv1thicktime),
  p_fv1thicktemp(fv1thicktemp),
  p_fv2thicktime(fv2thicktime),
  p_fv2thicktemp(fv2thicktemp),
  p_nvthicktime(nvthicktime),
  p_nvthicktemp(nvthicktemp),
  p_tsec(tsec),
  p_tmin(tmin),
  p_hrrkw(hrrkw),
  p_hrrbtu(hrrbtu),
  p_detsprinktime(detsprinktime),
  p_detsmtime(detsmtime),
  p_detfthtime(detfthtime),
  p_flwallinehgt(flwallinehgt),
  p_flcornerhgt(flcornerhgt),
  p_flwallhgt(flwallhgt),
  p_hrrhrr(hrrhrr),
  p_hrrburndur(hrrburndur),
  p_hrrhgthesk(hrrhgthesk),
  p_hrrhgtthom(hrrhgtthom),
  p_pltemp(pltemp),
  p_tcltemp(tcltemp),
  p_visdist(visdist),
  p_fv1thintemp(fv1thintemp),
  p_fv2thintemp(fv2thintemp),
  p_nvthintemp(nvthintemp)
{
   (*p_fuelselindex) = 1;
   (*p_matselindex) = 1;
   (*p_vismatselindex) = 1;
   (*p_durmatselindex) = 1;
   (*p_vispropselindex) = 1;
   (*p_viscombselindex) = 1;
   (*p_detrtiselindex) = 1;
   (*p_dettempratselindex) = 1;
   (*p_detspaceselindex) = 1;
   (*p_cableselindex) = 1;
   (*p_tempmethod) = 0;
   (*p_tempcalcmethod) = 0;
   (*p_detectortype) = 0;
   (*p_detacttemp) = 0;
   (*p_flametype) = 0;
   (*p_killexcel) = 0;
   //(*p_tsec) = 0;
   (*p_tmin) = 0;
   (*p_hrrkw) = 0;
   (*p_hrrbtu) = 0;
   (*p_detsprinktime) = 0;
   (*p_detsmtime) = 0;
   (*p_detfthtime) = 0;
   (*p_flwallinehgt) = 0;
   (*p_flcornerhgt) = 0;
   (*p_flwallhgt) = 0;
   (*p_hrrhrr) = 0;
   (*p_hrrburndur) = 0;
   (*p_hrrhgthesk) = 0;
   (*p_hrrhgtthom) = 0;
   (*p_pltemp) = 0;
   (*p_tcltemp) = 0;
   (*p_visdist) = 0;
   times = 0;

   _buildPage();
}

/////////////////////////////////////////////////////
OPPD_UI_Dialog
::~OPPD_UI_Dialog()
{
	//delete _tabs;
}

/////////////////////////////////////////////////////
void OPPD_UI_Dialog::_buildPage()
{
  _tabs = new OPPD_UI_tabs(this);

  _tabs->createTabPages();

  //_tabsSizer = new wxNotebookSizer(_tabs);
  wxBoxSizer* _tabsSizer = new wxBoxSizer(wxHORIZONTAL);
  _tabsSizer->Add(_tabs,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
  _tabsSizer->Layout();
   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(_tabsSizer);
   _tabsSizer->Fit(this);  
}



/////////////////////////////////////////////////////
bool OPPD_UI_Dialog::TransferDataFromWindow()
{
	wxString txt;

	(*p_fuelpardbls).clear();
	txt = _tabs->_entriesPage->_spillvol->GetValue();
	(*p_fuelpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_spillarea->GetValue();
	(*p_fuelpardbls).push_back(atof(txt.c_str()));

	(*p_compardbls).clear();
	txt = _tabs->_entriesPage->_comwidth->GetValue();
	(*p_compardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_comlength->GetValue();
	(*p_compardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_comheight->GetValue();
	(*p_compardbls).push_back(atof(txt.c_str()));

	txt  = _tabs->_entriesPage->_intlinthick->GetValue();
	(*p_intlinthickdbl) = atof(txt.c_str());
	
	txt = _tabs->_entriesPage->_massfuelburn->GetValue();
	(*p_massfuelburndbl) = atof(txt.c_str());
	txt = _tabs->_entriesPage->_solidfuelarea->GetValue();
	(*p_solidfuelareadbl) = atof(txt.c_str());
	
	(*p_ambpardbls).clear();
	txt = _tabs->_entriesPage->_airtemp->GetValue();
	(*p_ambpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_spheatair->GetValue();
	(*p_ambpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_airdensity->GetValue();
	(*p_ambpardbls).push_back(atof(txt.c_str()));

	txt = _tabs->_entriesPage->_evalabovefire->GetValue();
	(*p_evalabvfiredbl) = atof(txt);

	(*p_ventpardbls).clear();
	txt = _tabs->_entriesPage->_ventwidth->GetValue();
	(*p_ventpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_ventheight->GetValue();
	(*p_ventpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_ventdisfloor->GetValue();
	(*p_ventpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_timeaftign->GetValue();
	(*p_ventpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_forcvntflowrt->GetValue();
	(*p_ventpardbls).push_back(atof(txt.c_str()));

	(*p_detectpardbls).clear();
	txt = _tabs->_entriesPage->_detdistoceil->GetValue();
	(*p_detectpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_detraddistospr->GetValue();
	(*p_detectpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_detceilheight->GetValue();
	(*p_detectpardbls).push_back(atof(txt.c_str()));

	txt = _tabs->_entriesPage->_cblburnarea->GetValue();
	(*p_cblburnareadbl) = atof(txt.c_str());
	
	(*p_tempmethod) = _tabs->_entriesPage->_tempscenRBox->GetSelection();
	(*p_tempcalcmethod) = _tabs->_entriesPage->_fvmethodRBox->GetSelection();
	(*p_detectortype) = _tabs->_entriesPage->_detactRBox->GetSelection();
	(*p_flametype) = _tabs->_entriesPage->_flamescenRBox->GetSelection();

  return true;
}

////////////////////////////////////////////////////
bool OPPD_UI_Dialog::TransferDataToWindow()
{
	
	/*wxString txt1,txt2,txt3,txt4,txt5,txt6,txt7,txt8,txt9,txt10,txt11,txt12,txt13,txt14,txt15,txt16,txt17;
	
	//txt1<<(*p_tsec);
	if (times == 0)
	{
		txt1<<"just testing"; 
		times = 1;
	}
	else 
	{
		txt1<<"2nd time"; 
		times = 0;
	}

	_tabs->_outputsPage->_tsec->Clear();
	_tabs->_outputsPage->_tsec->SetValue(txt1);

	txt2<<(*p_tmin); 
	_tabs->_outputsPage->_tmin->Clear();
	_tabs->_outputsPage->_tmin->SetValue(txt2);

	txt3<<(*p_hrrkw); 
	_tabs->_outputsPage->_hrrkw->Clear();
	_tabs->_outputsPage->_hrrkw->SetValue(txt3);

	txt4<<(*p_hrrbtu); 
	_tabs->_outputsPage->_hrrbtu->Clear();
	_tabs->_outputsPage->_hrrbtu->SetValue(txt4);

	txt5<<(*p_detsprinktime); 
	_tabs->_outputsPage->_detsprinktime->Clear();
	_tabs->_outputsPage->_detsprinktime->SetValue(txt5);

	txt6<<(*p_detsmtime); 
	_tabs->_outputsPage->_detsmtime->Clear();
	_tabs->_outputsPage->_detsmtime->SetValue(txt6);

	txt7<<(*p_detfthtime); 
	_tabs->_outputsPage->_detfthtime->Clear();
	_tabs->_outputsPage->_detfthtime->SetValue(txt7);

	txt8<<(*p_flwallinehgt); 
	_tabs->_outputsPage->_flwallinehgt->Clear();
	_tabs->_outputsPage->_flwallinehgt->SetValue(txt8);

	txt9<<(*p_flcornerhgt); 
	_tabs->_outputsPage->_flcornerhgt->Clear();
	_tabs->_outputsPage->_flcornerhgt->SetValue(txt9);

	txt10<<(*p_flwallhgt); 
	_tabs->_outputsPage->_flwallhgt->Clear();
	_tabs->_outputsPage->_flwallhgt->SetValue(txt10);

	txt11<<(*p_hrrhrr); 
	_tabs->_outputsPage->_hrrhrr->Clear();
	_tabs->_outputsPage->_hrrhrr->SetValue(txt11);

	txt12<<(*p_hrrburndur); 
	_tabs->_outputsPage->_hrrburndur->Clear();
	_tabs->_outputsPage->_hrrburndur->SetValue(txt12);

	txt13<<(*p_hrrhgthesk); 
	_tabs->_outputsPage->_hrrhgthesk->Clear();
	_tabs->_outputsPage->_hrrhgthesk->SetValue(txt13);

	txt14<<(*p_hrrhgtthom); 
	_tabs->_outputsPage->_hrrhgtthom->Clear();
	_tabs->_outputsPage->_hrrhgtthom->SetValue(txt14);

	txt15<<(*p_pltemp); 
	_tabs->_outputsPage->_pltemp->Clear();
	_tabs->_outputsPage->_pltemp->SetValue(txt15);

	txt16<<(*p_tcltemp); 
	_tabs->_outputsPage->_tcltemp->Clear();
	_tabs->_outputsPage->_tcltemp->SetValue(txt16);

	txt17<<(*p_visdist); 
	_tabs->_outputsPage->_visdist->Clear();
	_tabs->_outputsPage->_visdist->SetValue(txt17);
*/
    return true;
}

void OPPD_UI_Dialog::Lock(bool l)
{
}
