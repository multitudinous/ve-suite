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
  vector<double>* detectpardbls)
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
  p_detectpardbls(detectpardbls)
{
   (*p_fuelselindex) = 0;
   (*p_matselindex) = 0;
   (*p_vismatselindex) = 0;
   (*p_durmatselindex) = 0;
   (*p_vispropselindex) = 0;
   (*p_viscombselindex) = 0;
   (*p_detrtiselindex) = 0;
   (*p_dettempratselindex) = 0;
   (*p_detspaceselindex) = 0;
   (*p_cableselindex) = 0;
   (*p_tempmethod) = 0;
   (*p_tempcalcmethod) = 0;
   (*p_detectortype) = 0;
   (*p_detacttemp) = 0;
   (*p_flametype) = 0;
   (*p_killexcel) = 0;

   _buildPage();
}

/////////////////////////////////////////////////////
OPPD_UI_Dialog
::~OPPD_UI_Dialog()
{
	delete _tabs;
}

/////////////////////////////////////////////////////
void OPPD_UI_Dialog::_buildPage()
{
  _tabs = new OPPD_UI_tabs(this);

  _tabs->createTabPages();

  wxNotebookSizer* _tabsSizer = new wxNotebookSizer(_tabs);

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

	txt = _tabs->_entriesPage->_spillvol->GetValue();
	(*p_fuelpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_spillarea->GetValue();
	(*p_fuelpardbls).push_back(atof(txt.c_str()));

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
	
	txt = _tabs->_entriesPage->_airtemp->GetValue();
	(*p_ambpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_spheatair->GetValue();
	(*p_ambpardbls).push_back(atof(txt.c_str()));
	txt = _tabs->_entriesPage->_airdensity->GetValue();
	(*p_ambpardbls).push_back(atof(txt.c_str()));

	txt = _tabs->_entriesPage->_evalabovefire->GetValue();
	(*p_evalabvfiredbl) = atof(txt);

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
    return true;
}

void OPPD_UI_Dialog::Lock(bool l)
{
}
