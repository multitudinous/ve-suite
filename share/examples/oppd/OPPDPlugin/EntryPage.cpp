#include "EntryPage.h"

BEGIN_EVENT_TABLE(EntryPage, wxPanel)
   EVT_RADIOBOX		(FUEL_TEMPSCEN_RADIOBOX,		EntryPage::_onTempScen)
   EVT_RADIOBOX		(FUEL_FVMETHOD_RADIOBOX,		EntryPage::_onFVMethod)
   EVT_RADIOBOX		(FUEL_FLAMESCEN_RADIOBOX,		EntryPage::_onFlameScen)
   EVT_RADIOBOX		(FUEL_DETACT_RADIOBOX,			EntryPage::_onDetAct)
   EVT_BUTTON		(FUEL_KILLEXCEL_BUTTON,			EntryPage::_onKillExcel)
   EVT_BUTTON		(FUEL_RESETSHEET_BUTTON,		EntryPage::_onResetSheet)
   EVT_COMBOBOX		(FUEL_FUELSEL_COMBOBOX,			EntryPage::_onFuelSelect)
   EVT_COMBOBOX		(FUEL_MATSEL_COMBOBOX,			EntryPage::_onMatSelect)
   EVT_COMBOBOX		(FUEL_VISMATSEL_COMBOBOX,		EntryPage::_onVismatSelect)
   EVT_COMBOBOX		(FUEL_DURMATSEL_COMBOBOX,		EntryPage::_onDurmatSelect)
   EVT_COMBOBOX		(FUEL_VISPROPSEL_COMBOBOX,		EntryPage::_onVispropSelect)
   EVT_COMBOBOX		(FUEL_VISCOMBSEL_COMBOBOX,		EntryPage::_onViscombSelect)
   EVT_COMBOBOX		(FUEL_DETRTISEL_COMBOBOX,		EntryPage::_onDetrtiSelect)
   EVT_COMBOBOX		(FUEL_DETTEMPRATSEL_COMBOBOX,	EntryPage::_onDettempratSelect)
   EVT_RADIOBOX		(FUEL_DETACTTEMP_RADIOBOX,		EntryPage::_onDetActTemp)
   EVT_COMBOBOX		(FUEL_DETSPACESEL_COMBOBOX,		EntryPage::_onDetSpaceSelect)
   EVT_COMBOBOX		(FUEL_CABLESEL_COMBOBOX,		EntryPage::_onCableSelect)
   //EVT_BUTTON		(FUEL_UPDATE_BUTTON,			EntryPage::_onUpdate)
END_EVENT_TABLE()


EntryPage::EntryPage(wxNotebook* tControl)
:wxPanel(tControl)
{
   _tempscenRBox = 0;
   _fvmethodRBox = 0;
   _flamescenRBox = 0;
   _detactRBox = 0;
   _killexcelButton = 0;
   _resetsheetButton = 0;
   _updateButton = 0;
   noULlistings = 8;

   _buildPage();
   _createfuelCombo();
   _creatematCombo();
   _createvismatCombo();
   _createdurmatCombo();
   _createvispropCombo();
   _createviscombCombo();
   _createdetrtiCombo();
   _createdettempratCombo();
   _createdetspaceCombo();
   _createcableCombo();
}

void EntryPage::_buildPage()
{
   //The names of the radio box choices
   wxString tempscenstr[] = { wxT("Closed Door"),
							wxT("Forced Ventilation: Thermally Thick"),
                            wxT("Forced Ventilation: Thermally Thin"),
							wxT("Natural Ventilation: Thermally Thick"),
							wxT("Natural Ventilation: Thermally Thin")};

   _tempscenRBox = new wxRadioBox(this, FUEL_TEMPSCEN_RADIOBOX, wxT("Select The Appropriate Temperature Scenario"),
                                                  wxDefaultPosition, wxDefaultSize, 5,
                                                     tempscenstr, 1, wxRA_SPECIFY_COLS);

    wxString fvmethodstr[] = { wxT("Deal and Beyler Method"),
							 wxT("Foote, Pagni, and Alvares Method")};

   _fvmethodRBox = new wxRadioBox(this, FUEL_FVMETHOD_RADIOBOX, wxT("For Forced Ventilation, Select the Calculation Method"),
                                                  wxDefaultPosition, wxDefaultSize, 2,
                                                     fvmethodstr, 1, wxRA_SPECIFY_COLS);
	_fvmethodRBox->Enable( false );
   //The names of the radio box choices
   wxString flamescenstr[] = { wxT("Wall Line Flame"),
							wxT("Corner Flame"),
							wxT("Wall Flame")};

   _flamescenRBox = new wxRadioBox(this, FUEL_FLAMESCEN_RADIOBOX, wxT("Select The Flame Type"),
                                                  wxDefaultPosition, wxDefaultSize, 3,
                                                     flamescenstr, 1, wxRA_SPECIFY_COLS);


   wxString detactstr[] = { wxT("Sprinkler"),
							wxT("Smoke"),
							wxT("FTH Detector")};

   _detactRBox = new wxRadioBox(this, FUEL_DETACT_RADIOBOX, wxT("Select The Detector"),
                                                  wxDefaultPosition, wxDefaultSize, 3,
                                                     detactstr, 1, wxRA_SPECIFY_COLS);

   wxString detacttempstr[] = { wxT("128"),
							    wxT("135"),
							    wxT("145"),
								wxT("160"),
								wxT("170"),
								wxT("196")};

   _detacttemRBox = new wxRadioBox(this, FUEL_DETACTTEMP_RADIOBOX, wxT("Select The Detector Activation Temperature"),
                                                  wxDefaultPosition, wxDefaultSize, 6,
                                                     detacttempstr, 6, wxRA_SPECIFY_COLS);
   //the input boxes
   _fuelsel       = new wxComboBox(this,FUEL_FUELSEL_COMBOBOX , wxT("Select Fuel"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _spillvol      = new wxTextCtrl(this, -1, wxT("5.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _spillarea     = new wxTextCtrl(this, -1, wxT("8.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _comwidth      = new wxTextCtrl(this, -1, wxT("40"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _comlength     = new wxTextCtrl(this, -1, wxT("40"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _comheight     = new wxTextCtrl(this, -1, wxT("12"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _matsel        = new wxComboBox(this,FUEL_MATSEL_COMBOBOX , wxT("Select Material"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _intlinthick   = new wxTextCtrl(this, -1, wxT("0.25"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _airtemp       = new wxTextCtrl(this, -1, wxT("77"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _spheatair     = new wxTextCtrl(this, -1, wxT("1.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _airdensity    = new wxTextCtrl(this, -1, wxT("1.20"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _massfuelburn  = new wxTextCtrl(this, -1, wxT("0.5"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _solidfuelarea = new wxTextCtrl(this, -1, wxT("10"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _evalabovefire = new wxTextCtrl(this, -1, wxT("20"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _ventwidth     = new wxTextCtrl(this, -1, wxT("6.50"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _ventheight    = new wxTextCtrl(this, -1, wxT("5.60"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _ventdisfloor  = new wxTextCtrl(this, -1, wxT("8.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _timeaftign    = new wxTextCtrl(this, -1, wxT("8.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _forcvntflowrt = new wxTextCtrl(this, -1, wxT("400"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _vismatsel     = new wxComboBox(this,FUEL_VISMATSEL_COMBOBOX , wxT("Select Solid Fuel"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _durmatsel     = new wxComboBox(this,FUEL_DURMATSEL_COMBOBOX , wxT("Select Solid Fuel"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _vispropsel    = new wxComboBox(this,FUEL_VISPROPSEL_COMBOBOX , wxT("Select Situation"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _viscombsel    = new wxComboBox(this,FUEL_VISCOMBSEL_COMBOBOX , wxT("Select Mode of Combustion"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _detrtisel     = new wxComboBox(this,FUEL_DETRTISEL_COMBOBOX , wxT("Select Sprinkler Type"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _dettempratsel = new wxComboBox(this,FUEL_DETTEMPRATSEL_COMBOBOX , wxT("Select Temperature Classification"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _detdistoceil  = new wxTextCtrl(this, -1, wxT("10.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _detraddistospr= new wxTextCtrl(this, -1, wxT("10.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _detceilheight = new wxTextCtrl(this, -1, wxT("20.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _detspacesel   = new wxComboBox(this,FUEL_DETSPACESEL_COMBOBOX , wxT("Select Detector Spacing"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   _cblburnarea   = new wxTextCtrl(this, -1, wxT("10.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _cablesel      = new wxComboBox(this,FUEL_CABLESEL_COMBOBOX , wxT("Select Cable Type"),wxDefaultPosition, wxDefaultSize,0,NULL, wxCB_DROPDOWN);
   
   //build the labels
   wxStaticText* fuelselLabel       = new wxStaticText(this, -1, wxT("Fuel Selection "));
   wxStaticText* spillvolLabel      = new wxStaticText(this, -1, wxT("Fuel Spill Volume "));
   wxStaticText* spillareaLabel     = new wxStaticText(this, -1, wxT("Fuel Spill Area "));
   wxStaticText* comwidthLabel      = new wxStaticText(this, -1, wxT("Compartment Width "));
   wxStaticText* comlengthLabel     = new wxStaticText(this, -1, wxT("Compartment Length "));
   wxStaticText* comheightLabel     = new wxStaticText(this, -1, wxT("Compartment Height "));
   wxStaticText* matselLabel        = new wxStaticText(this, -1, wxT("Material Selection "));
   wxStaticText* intlinthickLabel   = new wxStaticText(this, -1, wxT("Interior Lining Thickness "));
   wxStaticText* airtempLabel       = new wxStaticText(this, -1, wxT("Air Temperature "));
   wxStaticText* spheatairLabel     = new wxStaticText(this, -1, wxT("Specific Heat Air "));
   wxStaticText* airdensityLabel    = new wxStaticText(this, -1, wxT("Air Density "));
   wxStaticText* massfuelburnLabel  = new wxStaticText(this, -1, wxT("Mass of Solid Fuel Burn "));
   wxStaticText* solidfuelareaLabel = new wxStaticText(this, -1, wxT("Surface Area of Solid Fuel "));
   wxStaticText* vismatselLabel     = new wxStaticText(this, -1, wxT("Solid Fuel Selection(Particulate Yield Table) "));
   wxStaticText* durmatselLabel     = new wxStaticText(this, -1, wxT("Solid Fuel Selection(HRR,Heat of Comb. Table) "));
   wxStaticText* vispropselLabel    = new wxStaticText(this, -1, wxT("Light Situation Selection "));
   wxStaticText* viscombselLabel    = new wxStaticText(this, -1, wxT("Combustion Mode Selection "));
   wxStaticText* evalabovefireLabel = new wxStaticText(this, -1, wxT("Evaluation Above Fire Source "));
   wxStaticText* ventwidthLabel     = new wxStaticText(this, -1, wxT("Vent Width(if natural ventilation) "));
   wxStaticText* ventheightLabel    = new wxStaticText(this, -1, wxT("Vent Height(if natural ventilation) "));
   wxStaticText* ventdisfloorLabel  = new wxStaticText(this, -1, wxT("Top of Vent from Floor "));
   wxStaticText* frcvntflwrtLabel   = new wxStaticText(this, -1, wxT("Forced Ventilation Flow Rate(if used) "));
   wxStaticText* timeaftignLabel    = new wxStaticText(this, -1, wxT("Time After Ignition "));
   wxStaticText* detrtiselLabel     = new wxStaticText(this, -1, wxT("Sprinkler Type Selection "));
   wxStaticText* dettempratselLabel = new wxStaticText(this, -1, wxT("Temperature Classification Selection "));
   wxStaticText* dtdstoceilLabel    = new wxStaticText(this, -1, wxT("Dist from Top of Fuel Package to Ceiling "));
   wxStaticText* dtrdsttosprLabel   = new wxStaticText(this, -1, wxT("Radial Dist from Plume Centerline to Sprinkler/Detector "));
   wxStaticText* detceilheightLabel = new wxStaticText(this, -1, wxT("Ceiling Height "));
   wxStaticText* detspaceselLabel   = new wxStaticText(this, -1, wxT("Detector Spacing Selection "));
   wxStaticText* cblburnareaLabel   = new wxStaticText(this, -1, wxT("Exposed Cable Tray Burning Area "));
   wxStaticText* cableselLabel      = new wxStaticText(this, -1, wxT("Cable Type Selection "));

   //attach the labels to the boxes
   wxBoxSizer* fuel1Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* fuel2Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* fuel3Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* fuel4Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* fuel5Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* fuel6Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* fuel7Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* fuel8Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* amb1Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* amb2Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* amb3Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vis1Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vis2Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vis3Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vis4Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vis5Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vis6Group   = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* plumeGroup  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vent1Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vent2Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vent3Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vent4Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* vent5Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* detect1Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* detect2Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* detect3Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* detect4Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* detect5Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* detect6Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* cable1Group  = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* cable2Group  = new wxBoxSizer(wxHORIZONTAL);


   fuel1Group->Add(fuelselLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel1Group->Add(_fuelsel,0,wxALIGN_RIGHT|wxEXPAND);

   fuel2Group->Add(spillvolLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel2Group->Add(_spillvol,0,wxALIGN_RIGHT|wxEXPAND);

   fuel3Group->Add(spillareaLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel3Group->Add(_spillarea,0,wxALIGN_RIGHT|wxEXPAND);

   fuel4Group->Add(comwidthLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel4Group->Add(_comwidth,0,wxALIGN_RIGHT|wxEXPAND);

   fuel5Group->Add(comlengthLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel5Group->Add(_comlength,0,wxALIGN_RIGHT|wxEXPAND);

   fuel6Group->Add(comheightLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel6Group->Add(_comheight,0,wxALIGN_RIGHT|wxEXPAND);

   fuel7Group->Add(matselLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel7Group->Add(_matsel,0,wxALIGN_RIGHT|wxEXPAND);

   fuel8Group->Add(intlinthickLabel,1,wxALIGN_LEFT|wxEXPAND);
   fuel8Group->Add(_intlinthick,0,wxALIGN_RIGHT|wxEXPAND);

   amb1Group->Add(airtempLabel,1,wxALIGN_LEFT|wxEXPAND);
   amb1Group->Add(_airtemp,0,wxALIGN_RIGHT|wxEXPAND);

   amb2Group->Add(spheatairLabel,1,wxALIGN_LEFT|wxEXPAND);
   amb2Group->Add(_spheatair,0,wxALIGN_RIGHT|wxEXPAND);

   amb3Group->Add(airdensityLabel,1,wxALIGN_LEFT|wxEXPAND);
   amb3Group->Add(_airdensity,0,wxALIGN_RIGHT|wxEXPAND);

   vis1Group->Add(massfuelburnLabel,1,wxALIGN_LEFT|wxEXPAND);
   vis1Group->Add(_massfuelburn,0,wxALIGN_RIGHT|wxEXPAND);

   vis2Group->Add(solidfuelareaLabel,1,wxALIGN_LEFT|wxEXPAND);
   vis2Group->Add(_solidfuelarea,0,wxALIGN_RIGHT|wxEXPAND);

   vis3Group->Add(durmatselLabel,1,wxALIGN_LEFT|wxEXPAND);
   vis3Group->Add(_durmatsel,0,wxALIGN_RIGHT|wxEXPAND);

   vis4Group->Add(vismatselLabel,1,wxALIGN_LEFT|wxEXPAND);
   vis4Group->Add(_vismatsel,0,wxALIGN_RIGHT|wxEXPAND);

   vis5Group->Add(vispropselLabel,1,wxALIGN_LEFT|wxEXPAND);
   vis5Group->Add(_vispropsel,0,wxALIGN_RIGHT|wxEXPAND);

   vis6Group->Add(viscombselLabel,1,wxALIGN_LEFT|wxEXPAND);
   vis6Group->Add(_viscombsel,0,wxALIGN_RIGHT|wxEXPAND);

   plumeGroup->Add(evalabovefireLabel,1,wxALIGN_LEFT|wxEXPAND);
   plumeGroup->Add(_evalabovefire,0,wxALIGN_RIGHT|wxEXPAND);

   vent1Group->Add(ventwidthLabel,1,wxALIGN_LEFT|wxEXPAND);
   vent1Group->Add(_ventwidth,0,wxALIGN_RIGHT|wxEXPAND);

   vent2Group->Add(ventheightLabel,1,wxALIGN_LEFT|wxEXPAND);
   vent2Group->Add(_ventheight,0,wxALIGN_RIGHT|wxEXPAND);

   vent3Group->Add(ventdisfloorLabel,1,wxALIGN_LEFT|wxEXPAND);
   vent3Group->Add(_ventdisfloor,0,wxALIGN_RIGHT|wxEXPAND);

   vent4Group->Add(timeaftignLabel,1,wxALIGN_LEFT|wxEXPAND);
   vent4Group->Add(_timeaftign,0,wxALIGN_RIGHT|wxEXPAND);

   vent5Group->Add(frcvntflwrtLabel,1,wxALIGN_LEFT|wxEXPAND);
   vent5Group->Add(_forcvntflowrt,0,wxALIGN_RIGHT|wxEXPAND);

   detect1Group->Add(detrtiselLabel,1,wxALIGN_LEFT|wxEXPAND);
   detect1Group->Add(_detrtisel,0,wxALIGN_RIGHT|wxEXPAND);

   detect2Group->Add(dettempratselLabel,1,wxALIGN_LEFT|wxEXPAND);
   detect2Group->Add(_dettempratsel,0,wxALIGN_RIGHT|wxEXPAND);

   detect3Group->Add(dtdstoceilLabel,1,wxALIGN_LEFT|wxEXPAND);
   detect3Group->Add(_detdistoceil,0,wxALIGN_RIGHT|wxEXPAND);

   detect4Group->Add(dtrdsttosprLabel,1,wxALIGN_LEFT|wxEXPAND);
   detect4Group->Add(_detraddistospr,0,wxALIGN_RIGHT|wxEXPAND);

   detect5Group->Add(detceilheightLabel,1,wxALIGN_LEFT|wxEXPAND);
   detect5Group->Add(_detceilheight,0,wxALIGN_RIGHT|wxEXPAND);

   detect6Group->Add(detspaceselLabel,1,wxALIGN_LEFT|wxEXPAND);
   detect6Group->Add(_detspacesel,0,wxALIGN_RIGHT);

   cable1Group->Add(cblburnareaLabel,1,wxALIGN_LEFT|wxEXPAND);
   cable1Group->Add(_cblburnarea,0,wxALIGN_RIGHT|wxEXPAND);

   cable2Group->Add(cableselLabel,1,wxALIGN_LEFT|wxEXPAND);
   cable2Group->Add(_cablesel,0,wxALIGN_RIGHT|wxEXPAND);

   //the update button
   _killexcelButton			= new wxButton(this,FUEL_KILLEXCEL_BUTTON,wxT("Close Spreadsheets"));
   _resetsheetButton		= new wxButton(this,FUEL_RESETSHEET_BUTTON,wxT("Reset Spreadsheets"));
   //_updateButton		    = new wxButton(this,FUEL_UPDATE_BUTTON,wxT("Update Fire Scenario Data"));
   _updateButton		    = new wxButton(this,wxID_OK,wxT("Update Fire Scenario Data"));

   //the panel sizer
   wxStaticBox* fuelBox = new wxStaticBox(this, -1, "FUEL/COMPARTMENT PARAMETERS");
   wxStaticBoxSizer* fuelPanelGroup = new wxStaticBoxSizer(fuelBox,wxVERTICAL);
   fuelPanelGroup->Add(fuel1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   fuelPanelGroup->Add(fuel2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   fuelPanelGroup->Add(fuel3Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //fuelPanelGroup->Add(_updatefuelButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   fuelPanelGroup->Add(fuel4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   fuelPanelGroup->Add(fuel5Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   fuelPanelGroup->Add(fuel6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   fuelPanelGroup->Add(fuel7Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   fuelPanelGroup->Add(fuel8Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //fuelPanelGroup->Add(_updatecompButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBox* visibBox = new wxStaticBox(this, -1, "SOLID FUEL/VISIBILITY PARAMETERS");
   wxStaticBoxSizer* visibPanelGroup = new wxStaticBoxSizer(visibBox, wxVERTICAL);
   visibPanelGroup->Add(vis1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   visibPanelGroup->Add(vis2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   visibPanelGroup->Add(vis3Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   visibPanelGroup->Add(vis4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   visibPanelGroup->Add(vis5Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   visibPanelGroup->Add(vis6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //visibPanelGroup->Add(_updatevisibButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBox* ambientBox = new wxStaticBox(this, -1, "AMBIENT CONDITIONS");
   wxStaticBoxSizer* ambientPanelGroup = new wxStaticBoxSizer(ambientBox, wxVERTICAL);
   ambientPanelGroup->Add(amb1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   ambientPanelGroup->Add(amb2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   ambientPanelGroup->Add(amb3Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //ambientPanelGroup->Add(_updateambientButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBox* plumeBox = new wxStaticBox(this, -1, "PLUME TEMP PARAMETERS");
   wxStaticBoxSizer* plumePanelGroup = new wxStaticBoxSizer(plumeBox, wxVERTICAL);
   plumePanelGroup->Add(plumeGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //plumePanelGroup->Add(_updateplumeButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBox* ventBox = new wxStaticBox(this, -1, "VENTILATION PARAMETERS");
   wxStaticBoxSizer* ventPanelGroup = new wxStaticBoxSizer(ventBox, wxVERTICAL);
   ventPanelGroup->Add(vent1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   ventPanelGroup->Add(vent2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   ventPanelGroup->Add(vent3Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   ventPanelGroup->Add(vent4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   ventPanelGroup->Add(vent5Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //ventPanelGroup->Add(_updateventButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBox* detectFTHBox = new wxStaticBox(this, -1, "FTH DETECTOR SETTINGS");
   wxStaticBoxSizer* detectFTHPanelGroup = new wxStaticBoxSizer(detectFTHBox, wxVERTICAL);
   detectFTHPanelGroup->Add(_detacttemRBox,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectFTHPanelGroup->Add(detect6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBox* detectBox = new wxStaticBox(this, -1, "DETECTOR ACTIVATION PARAMETERS");
   wxStaticBoxSizer* detectPanelGroup = new wxStaticBoxSizer(detectBox, wxVERTICAL);
   detectPanelGroup->Add(detect1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect3Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect5Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detectFTHPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //detectPanelGroup->Add(_updatedetectorButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* tempradioGroup = new wxBoxSizer(wxVERTICAL);
   tempradioGroup->Add(_tempscenRBox,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   tempradioGroup->Add(_fvmethodRBox,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* scenradioGroup = new wxBoxSizer(wxHORIZONTAL);
   scenradioGroup->Add(tempradioGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   scenradioGroup->Add(_flamescenRBox,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   scenradioGroup->Add(_detactRBox,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* scenbuttonGroup = new wxBoxSizer(wxHORIZONTAL);
   scenbuttonGroup->Add(_killexcelButton,0,wxALIGN_CENTER_HORIZONTAL);
   scenbuttonGroup->Add(_resetsheetButton,0,wxALIGN_CENTER_HORIZONTAL);

   wxStaticBox* scenarioBox = new wxStaticBox(this, -1, "ESTABLISH THE CALCULATION SCENARIOS");
   wxStaticBoxSizer* scenarioGroup = new wxStaticBoxSizer(scenarioBox,wxVERTICAL);
   scenarioGroup->Add(scenradioGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   scenarioGroup->Add(scenbuttonGroup,0,wxALIGN_CENTER_HORIZONTAL);

   wxStaticBox* cableBox = new wxStaticBox(this, -1, "CABLE PARAMETERS");
   wxStaticBoxSizer* cablePanelGroup = new wxStaticBoxSizer(cableBox,wxVERTICAL);
   cablePanelGroup->Add(cable1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   cablePanelGroup->Add(cable2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //cablePanelGroup->Add(_updatecableButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);


   wxBoxSizer* leftLayout = new wxBoxSizer(wxVERTICAL);
   leftLayout->Add(fuelPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   leftLayout->Add(visibPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   leftLayout->Add(cablePanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   leftLayout->Add(_updateButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* rightLayout = new wxBoxSizer(wxVERTICAL);
   rightLayout->Add(ambientPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   rightLayout->Add(plumePanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   rightLayout->Add(ventPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   rightLayout->Add(detectPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxBoxSizer* Row1 = new wxBoxSizer(wxHORIZONTAL);
   Row1->Add(scenarioGroup,0,wxALIGN_CENTER_HORIZONTAL);

   wxBoxSizer* Row2 = new wxBoxSizer(wxHORIZONTAL);
   Row2->Add(leftLayout,0,wxALIGN_CENTER_HORIZONTAL);
   Row2->Add(rightLayout,0,wxALIGN_CENTER_HORIZONTAL);

   wxBoxSizer* pageLayout = new wxBoxSizer(wxVERTICAL);
   pageLayout->Add(Row1,0,wxALIGN_CENTER_HORIZONTAL);
   pageLayout->Add(Row2,0,wxALIGN_CENTER_HORIZONTAL);
   //pageLayout->Add(Row3,0,wxALIGN_CENTER_HORIZONTAL);
   //pageLayout->Add(Row4,0,wxALIGN_CENTER_HORIZONTAL);


   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(pageLayout);
   pageLayout->Fit(this);  
}


void EntryPage::_createfuelCombo()
{
	_fuelsel->Clear();

	fuels.push_back("Methanol");
	fuels.push_back("Ethanol");
	fuels.push_back("Butane");
	fuels.push_back("Benzene");
	fuels.push_back("Hexane");
	fuels.push_back("Heptane");
	fuels.push_back("Xylene");
	fuels.push_back("Acetone");
	fuels.push_back("Dioxane");
	fuels.push_back("Diethy Ether");
	fuels.push_back("Benzine");
	fuels.push_back("Gasoline");
	fuels.push_back("Kerosine");
	fuels.push_back("Diesel");
	fuels.push_back("JP-4");
	fuels.push_back("JP-5");
	fuels.push_back("Transformer Oil, Hydrocarbon");
	fuels.push_back("Fuel Oil, Heavy");
	fuels.push_back("Crude Oil");
	fuels.push_back("Lube Oil");

	for(int i=0;i<20;i++)
		_fuelsel->Append(fuels[i]);
}

void EntryPage::_creatematCombo()
{
	_matsel->Clear();

	materials.push_back("Aluminum (pure)");
	materials.push_back("Steel (0.5% Carbon)");
	materials.push_back("Concrete");
	materials.push_back("Brick");
	materials.push_back("Glass, Plate");
	materials.push_back("Brick/Concrete Block");
	materials.push_back("Gypsum Board");
	materials.push_back("Plywood");
	materials.push_back("Fiber Insulation Board");
	materials.push_back("Chipboard");
	materials.push_back("Aerated Concrete");
	materials.push_back("Plasterboard");
	materials.push_back("Calcium Silicate Board");
	materials.push_back("Alumina Silicate Block");
	materials.push_back("Glass Fiber Insulation");
	materials.push_back("Expanded Polystyrene");

	for(int i=0;i<16;i++)
		_matsel->Append(materials[i]);
}

void EntryPage::_createvismatCombo()
{
	_vismatsel->Clear();

	vismats.push_back("Wood (Red Oak)");
	vismats.push_back("Wood (Douglas Fir)");
	vismats.push_back("Wood (Hemlock)");
	vismats.push_back("Fiberboard");
	vismats.push_back("Wool 100%");
	vismats.push_back("Acrylonitrile-Butadiene-Styrene (ABS)");
	vismats.push_back("Polymethylemethacrylate (PMMA; PlexiglasTM)");
	vismats.push_back("Polypropylene");
	vismats.push_back("Polystyrene");
	vismats.push_back("Silicone");
	vismats.push_back("Polyester");
	vismats.push_back("Nylon");
	vismats.push_back("Silicone Rubber");
	vismats.push_back("Polyurethane Foam (Flexible)");
	vismats.push_back("Polyurethane Foam (Rigid)");
	vismats.push_back("Polystyrene Foam");
	vismats.push_back("Polyethylene Foam");
	vismats.push_back("Phenolic Foam");
	vismats.push_back("Polyethylene (PE)");
	vismats.push_back("Polyvinylchloride (PVC)");
	vismats.push_back("Ethylenetetrafluoroethylene (ETFE; TefzelTM)");
	vismats.push_back("Perfluoroalkoxy (PFA; TeflonTM)");
	vismats.push_back("Fluorinated Polyethylene-Polypropylene (FEP; TeflonTM)");
	vismats.push_back("Tetrafluoroethylene (TFE; TeflonTM)");

	for(int i=0;i<24;i++)
		_vismatsel->Append(vismats[i]);
}

void EntryPage::_createdurmatCombo()
{
	_durmatsel->Clear();

	durmats.push_back("PE/PVC");	
	durmats.push_back("XPE/FRXPE");	
	durmats.push_back("XPE/Neoprene");	
	durmats.push_back("PE, Nylon/PVC, Nylon");	
	durmats.push_back("Teflon");
	durmats.push_back("Douglas fir plywood");	
	durmats.push_back("Fire retardant treated plywood");	
	durmats.push_back("Particleboard, 19 mm thick");	
	durmats.push_back("Nylon 6/6");	
	durmats.push_back("Polymethlmethacrylate (PMMA)");	
	durmats.push_back("Polypropylene (PP)");	
	durmats.push_back("Polystyrene (PS)");
	durmats.push_back("Polyethylene (PE)");	
	durmats.push_back("Polycarbonate");	
	durmats.push_back("Polyurethane");	
	durmats.push_back("Polyvinyl Chloride (PVC) Flexible");	
	durmats.push_back("Strene-butadiene Copolymers (SBR)");	
	durmats.push_back("Ethylene Propylene Dien Rubber (EPDM)");	
	durmats.push_back("Empty Cartons 15 ft high");	
	durmats.push_back("Wood pallets, stacked 1.5 ft high");	
	durmats.push_back("Wood pallets, stacked 5 ft high");	
	durmats.push_back("Wood pallets, stacked 10 ft high");	

	for(int i=0;i<22;i++)
		_durmatsel->Append(durmats[i]);
}

void EntryPage::_createvispropCombo()
{
	_vispropsel->Clear();

	visprops.push_back("Illuminated Signs");
	visprops.push_back("Reflecting Signs");
	visprops.push_back("Building Components in Reflected Light");

	for(int i=0;i<3;i++)
		_vispropsel->Append(visprops[i]);
}

void EntryPage::_createviscombCombo()
{
	_viscombsel->Clear();

	viscomb.push_back("Smoldering Combustion");
	viscomb.push_back("Flaming Combustion");

	for(int i=0;i<2;i++)
		_viscombsel->Append(viscomb[i]);
}

void EntryPage::_createdetrtiCombo()
{
	_detrtisel->Clear();

	detrti.push_back("Standard response bulb");
	detrti.push_back("Standard response link");
	detrti.push_back("Quick response bulb");
	detrti.push_back("Quick response link");

	for(int i=0;i<4;i++)
		_detrtisel->Append(detrti[i]);
}

void EntryPage::_createdettempratCombo()
{
	_dettempratsel->Clear();

	dettemprat.push_back("Ordinary");
	dettemprat.push_back("Intermediate");
	dettemprat.push_back("High");
	dettemprat.push_back("Extra high");
	dettemprat.push_back("Very extra high");
	dettemprat.push_back("Ultra high");
	dettemprat.push_back("Ultra high");

	for(int i=0;i<7;i++)
		_dettempratsel->Append(dettemprat[i]);
}

void EntryPage::_createdetspaceCombo()
{
	_detspacesel->Clear();

	if( noULlistings == 8 )
	{
		detspace.push_back("10 ft");
		detspace.push_back("15 ft");
		detspace.push_back("20 ft");
		detspace.push_back("25 ft");
		detspace.push_back("30 ft");
		detspace.push_back("40 ft");
		detspace.push_back("50 ft");
		detspace.push_back("70 ft");
	}
	else if ( noULlistings == 6 )
	{
		detspace.push_back("10 ft");
		detspace.push_back("15 ft");
		detspace.push_back("20 ft");
		detspace.push_back("25 ft");
		detspace.push_back("30 ft");
		detspace.push_back("40 ft");
	}
	else if ( noULlistings == 5 )
	{
		detspace.push_back("10 ft");
		detspace.push_back("15 ft");
		detspace.push_back("20 ft");
		detspace.push_back("25 ft");
		detspace.push_back("30 ft");
	}
	else if ( noULlistings == 3 )
	{
		detspace.push_back("10 ft");
		detspace.push_back("15 ft");
		detspace.push_back("20 ft");
	}

	for(int i=0;i<noULlistings;i++)
			_detspacesel->Append(detspace[i]);
}


void EntryPage::_createcableCombo()
{
	_cablesel->Clear();

	cabletype.push_back("ld PE 1071(kW/m2)");
	cabletype.push_back("PE/PVC 589(kW/m2)");
	cabletype.push_back("XPE/FRXPE 475(kW/m2)");
	cabletype.push_back("PE/PVC 395(kW/m2)");
	cabletype.push_back("PE/PVC 359(kW/m2)");
	cabletype.push_back("XPE/Neoprene 354(kW/m2)");
	cabletype.push_back("PE, PP/Cl.S.PE 345(kW/m2)");
	cabletype.push_back("PE/PVC 312(kW/m2)");
	cabletype.push_back("XPE/Neoprene 302(kW/m2)");
	cabletype.push_back("PE, PP/Cl.S.PE 299(kW/m2)");
	cabletype.push_back("PE, PP/Cl.S.PE 271(kW/m2)");
	cabletype.push_back("FRXPE/Cl.S.PE 258(kW/m2)");
	cabletype.push_back("PE, Nylon/PVC, Nylon 231(kW/m2)");
	cabletype.push_back("PE, Nylon/PVC, Nylon 218(kW/m2)");
	cabletype.push_back("XPE/Cl.S.PE 204(kW/m2)");
	cabletype.push_back("Silicone, glass braid, asbestos 182(kW/m2)");
	cabletype.push_back("XPE/XPE 178(kW/m2)");
	cabletype.push_back("PE, PP/Cl.S.PE 177(kW/m2)");
	cabletype.push_back("Silicone, glass braid 128(kW/m2)");
	cabletype.push_back("Teflon 98(kW/m2)");

	for(int i=0;i<20;i++)
		_cablesel->Append(cabletype[i]);
}

void EntryPage::_onKillExcel(wxCommandEvent& event)
{
	//(*p_killexcel) = 1;
	/*Wrapper->loadExcel();
    Wrapper->updateSheet(tempmethod,tempcalcmethod,detectortype,flametype);

    _createfuelCombo();
    _creatematCombo();
    _createvismatCombo();
	_createdurmatCombo();
    _createvispropCombo();
    _createviscombCombo();
	_createdetrtiCombo();
	_createdettempratCombo();
	_createdetspaceCombo();
	_createcableCombo();*/
}

void EntryPage::_onResetSheet(wxCommandEvent& event)
{
	//Wrapper->updateSheet(tempmethod,tempcalcmethod,detectortype,flametype);
   /*_createfuelCombo();
   _creatematCombo();
   _createvismatCombo();
   _createdurmatCombo();
   _createvispropCombo();
   _createviscombCombo();
   _createdetrtiCombo();
   _createdettempratCombo();
   _createdetspaceCombo();
   _createcableCombo();*/
}

void EntryPage::_onTempScen(wxCommandEvent& event)
{
	if (_tempscenRBox->GetSelection() == 1 || _tempscenRBox->GetSelection() == 2)
		_fvmethodRBox->Enable( true );
	else
		_fvmethodRBox->Enable( false );
}

void EntryPage::_onFVMethod(wxCommandEvent& event)
{
}

void EntryPage::_onFlameScen(wxCommandEvent& event)
{
}

void EntryPage::_onDetAct(wxCommandEvent& event)
{
}


void EntryPage::_onDetActTemp(wxCommandEvent& event)
{
	(*p_detacttemp) = _detacttemRBox->GetSelection();

	if ((*p_detacttemp) < 3)
		noULlistings = 8;
	else if ((*p_detacttemp) == 3)
		noULlistings = 6;
	else if ((*p_detacttemp) == 4)
		noULlistings = 5;
	else if ((*p_detacttemp) == 5)
		noULlistings = 3;

	_createdetspaceCombo();
}
void EntryPage::_onFuelSelect(wxCommandEvent& event)
{
	for(int i=0;i<20;i++)
	{
		if( _fuelsel->GetStringSelection() == fuels[i])
			(*p_fuelselindex) = i+1;
	}
}


void EntryPage::_onMatSelect(wxCommandEvent& event)
{
	for(int i=0;i<16;i++)
	{
		if( _matsel->GetStringSelection() == materials[i])
			(*p_matselindex) = i+1;
	}
}

void EntryPage::_onVismatSelect(wxCommandEvent& event)
{
	for(int i=0;i<24;i++)
	{
		if( _vismatsel->GetStringSelection() == vismats[i])
			(*p_vismatselindex) = i+1;
	}
}

void EntryPage::_onDurmatSelect(wxCommandEvent& event)
{
	for(int i=0;i<22;i++)
	{
		if( _durmatsel->GetStringSelection() == durmats[i])
			(*p_durmatselindex) = i+1;
	}
}

void EntryPage::_onVispropSelect(wxCommandEvent& event)
{
	for(int i=0;i<3;i++)
	{
		if( _vispropsel->GetStringSelection() == visprops[i])
			(*p_vispropselindex) = i+1;
	}
}

void EntryPage::_onViscombSelect(wxCommandEvent& event)
{
	for(int i=0;i<2;i++)
	{
		if( _viscombsel->GetStringSelection() == viscomb[i])
			(*p_viscombselindex) = i+1;
	}
}

void EntryPage::_onDetrtiSelect(wxCommandEvent& event)
{
	for(int i=0;i<4;i++)
	{
		if( _detrtisel->GetStringSelection() == detrti[i])
			(*p_detrtiselindex) = i+1;
	}
}

void EntryPage::_onDettempratSelect(wxCommandEvent& event)
{
	for(int i=0;i<7;i++)
	{
		if( _dettempratsel->GetStringSelection() == dettemprat[i])
			(*p_dettempratselindex) = i+1;
	}
}

void EntryPage::_onDetSpaceSelect(wxCommandEvent& event)
{   
	for(int i=0;i<noULlistings;i++)
	{
		if( _detspacesel->GetStringSelection() == detspace[i])
			(*p_detspaceselindex) = i+1;
	}
}

void EntryPage::_onCableSelect(wxCommandEvent& event)
{
	for(int i=0;i<20;i++)
	{
		if( _cablesel->GetStringSelection() == cabletype[i])
			(*p_cableselindex) = i+1;
	}
}
void EntryPage::_onUpdate(wxCommandEvent& event)
{
	/*fuelspillparams[0] = _spillvol->GetValue();
	fuelspillparams[1] = _spillarea->GetValue();
	
	for (int i=0; i<2; i++)
		fuelpardbls[i] = atof(fuelspillparams[i]);

	comparams[0] = _comwidth->GetValue();
	comparams[1] = _comlength->GetValue();
	comparams[2] = _comheight->GetValue();
	intlinthick  = _intlinthick->GetValue();
	
	for (int i=0; i<3; i++)
		compardbls[i] = atof(comparams[i]);

	intlinthickdbl = atof(intlinthick);

	massfuelburn = _massfuelburn->GetValue();
	solidfuelarea = _solidfuelarea->GetValue();
	massfuelburndbl = atof(massfuelburn);
	solidfuelareadbl = atof(solidfuelarea);

	ambparams[0] = _airtemp->GetValue();
	ambparams[1] = _spheatair->GetValue();
	ambparams[2] = _airdensity->GetValue();

	for (int i=0; i<3; i++)
		ambpardbls[i] = atof(ambparams[i]);

	evalabvfire = _evalabovefire->GetValue();
	evalabvfiredbl = atof(evalabvfire);

	ventparams[0] = _ventwidth->GetValue();
	ventparams[1] = _ventheight->GetValue();
	ventparams[2] = _ventdisfloor->GetValue();
	ventparams[3] = _timeaftign->GetValue();
	ventparams[4] = _forcvntflowrt->GetValue();
	
	for (int i=0; i<5; i++)
		ventpardbls[i] = atof(ventparams[i]);

	detectparams[0] = _detdistoceil->GetValue();
	detectparams[1] = _detraddistospr->GetValue();
	detectparams[2] = _detceilheight->GetValue();
	
	for (int i=0;i<3;i++)
		detectpardbls[i] = atof(detectparams[i]);

	cblburnarea = _cblburnarea->GetValue();
	cblburnareadbl = atof(cblburnarea);*/
}




