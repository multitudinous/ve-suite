#ifndef _ENTRY_PAGE_H
#define _ENTRY_PAGE_H
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>
#include <wx/notebook.h>
#include "EntryPage.h"
#include "ResultsPage.h"
#include <vector>
#include <string>

using namespace std;

enum {
   FUEL_TEMPSCEN_RADIOBOX,
   FUEL_FVMETHOD_RADIOBOX,
   FUEL_FLAMESCEN_RADIOBOX,
   FUEL_DETACT_RADIOBOX,
   FUEL_KILLEXCEL_BUTTON,
   FUEL_RESETSHEET_BUTTON,
   FUEL_MATSEL_COMBOBOX,
   FUEL_FUELSEL_COMBOBOX,
   FUEL_VISMATSEL_COMBOBOX,
   FUEL_DURMATSEL_COMBOBOX,
   FUEL_VISPROPSEL_COMBOBOX,
   FUEL_VISCOMBSEL_COMBOBOX,
   FUEL_DETRTISEL_COMBOBOX,
   FUEL_DETTEMPRATSEL_COMBOBOX,
   FUEL_DETACTTEMP_RADIOBOX,
   FUEL_DETSPACESEL_COMBOBOX,
   FUEL_CABLESEL_COMBOBOX
   //FUEL_UPDATE_BUTTON
};

class EntryPage: public wxPanel{
public:
	EntryPage(wxNotebook* tControl);

protected:
  //UI widgets variables
 wxRadioBox* _tempscenRBox;
 wxRadioBox* _fvmethodRBox;
 wxRadioBox* _flamescenRBox;
 wxRadioBox* _detactRBox;
 wxButton*   _killexcelButton;
 wxButton*   _resetsheetButton;
 wxComboBox* _fuelsel;
 wxTextCtrl* _spillvol;
 wxTextCtrl* _spillarea;
 wxTextCtrl* _comwidth;
 wxTextCtrl* _comlength;
 wxTextCtrl* _comheight;
 wxComboBox* _matsel;
 wxTextCtrl* _intlinthick;
 wxTextCtrl* _airtemp;
 wxTextCtrl* _spheatair;
 wxTextCtrl* _airdensity;
 wxTextCtrl* _massfuelburn;
 wxTextCtrl* _solidfuelarea;
 wxTextCtrl* _evalabovefire;
 wxTextCtrl* _ventwidth;
 wxTextCtrl* _ventheight;
 wxTextCtrl* _ventdisfloor;
 wxTextCtrl* _forcvntflowrt;
 wxTextCtrl* _timeaftign;
 wxComboBox* _vismatsel;
 wxComboBox* _durmatsel;
 wxComboBox* _vispropsel;
 wxComboBox* _viscombsel;
 wxComboBox* _detrtisel;
 wxComboBox* _dettempratsel;
 wxTextCtrl* _detdistoceil;
 wxTextCtrl* _detraddistospr;
 wxTextCtrl* _detceilheight;
 wxRadioBox* _detacttemRBox;
 wxComboBox* _detspacesel;
 wxTextCtrl* _cblburnarea;
 wxComboBox* _cablesel;
 wxButton*   _updateButton;

public:

  double* p_intlinthickdbl;
  double* p_massfuelburndbl;
  double* p_solidfuelareadbl;
  double* p_evalabvfiredbl;
  double* p_cblburnareadbl;
  long* p_tempmethod;
  long* p_tempcalcmethod;
  long* p_detectortype;
  long* p_flametype;
  long* p_detacttemp;
  long* p_matselindex;
  long* p_fuelselindex;
  long* p_vismatselindex;
  long* p_durmatselindex;
  long* p_vispropselindex;
  long* p_viscombselindex;
  long* p_detrtiselindex;
  long* p_dettempratselindex;
  long* p_detspaceselindex;
  long* p_cableselindex;
  long* p_killexcel;
  vector<double>* p_fuelpardbls;
  vector<double>* p_compardbls;
  vector<double>* p_ambpardbls;
  vector<double>* p_ventpardbls;
  vector<double>* p_detectpardbls;

    //GUI Variables

  vector<wxString> materials;
  vector<wxString> fuels;
  vector<wxString> vismats;
  vector<wxString> durmats;
  vector<wxString> visprops;
  vector<wxString> viscomb;
  vector<wxString> detrti;
  vector<wxString> dettemprat;
  vector<wxString> detspace;
  vector<wxString> cabletype;
  int noULlistings;

  void _buildPage();
  void _createfuelCombo();
  void _creatematCombo();
  void _createvismatCombo();
  void _createdurmatCombo();
  void _createvispropCombo();
  void _createviscombCombo();
  void _createdetrtiCombo();
  void _createdettempratCombo();
  void _createdetspaceCombo();
  void _createcableCombo();
  void _onKillExcel(wxCommandEvent& event);
  void _onResetSheet(wxCommandEvent& event);
  void _onTempScen(wxCommandEvent& event);
  void _onFVMethod(wxCommandEvent& event);
  void _onFlameScen(wxCommandEvent& event);
  void _onDetAct(wxCommandEvent& event);
  void _onDetActTemp(wxCommandEvent& event);
  void _onFuelSelect(wxCommandEvent& event);
  void _onMatSelect(wxCommandEvent& event);
  void _onVismatSelect(wxCommandEvent& event);
  void _onDurmatSelect(wxCommandEvent& event);
  void _onVispropSelect(wxCommandEvent& event);
  void _onViscombSelect(wxCommandEvent& event);
  void _onDetrtiSelect(wxCommandEvent& event);
  void _onDettempratSelect(wxCommandEvent& event);
  void _onDetSpaceSelect(wxCommandEvent& event);
  void _onCableSelect(wxCommandEvent& event);
  void _onUpdate(wxCommandEvent& event);

	DECLARE_EVENT_TABLE()
};
#endif //_ENTRY_PAGE_H_