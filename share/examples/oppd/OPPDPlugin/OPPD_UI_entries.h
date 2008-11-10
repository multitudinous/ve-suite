#ifndef _OPPD_UI_ENTRIES_H_
#define _OPPD_UI_ENTRIES_H_

#include "OPPD_UI.h"
#include <wx/wx.h>
#include <wx/notebook.h>

class OPPD_UI_Dialog;

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


class OPPD_UI_entries: public wxPanel
{
public:


	OPPD_UI_entries(wxNotebook* parent);

	~OPPD_UI_entries();

	OPPD_UI_Dialog* Dialog;

  //UI widgets variables
	wxRadioBox* _tempscenRBox;
	wxRadioBox* _fvmethodRBox;
	wxRadioBox* _flamescenRBox;
	wxRadioBox* _detactRBox;
	wxRadioBox* _detacttemRBox;
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
 wxComboBox* _detspacesel;
 wxTextCtrl* _cblburnarea;
 wxComboBox* _cablesel;
 wxButton*   _updateButton;

 wxNotebook* _parent;

  wxString materials[16];
  wxString fuels[20];
  wxString vismats[24];
  wxString durmats[22];
  wxString visprops[3];
  wxString viscomb[2];
  wxString detrti[4];
  wxString dettemprat[7];
  wxString detspace[8];
  wxString cabletype[20];
  int noULlistings;
protected:
  void _buildPage();
  void _createComboArrays();
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






	DECLARE_EVENT_TABLE();

};
#endif //_OPPD_UI_entries_H_