#include "OPPD_UI_entries.h"


BEGIN_EVENT_TABLE(OPPD_UI_entries, wxPanel)
   EVT_RADIOBOX		(FUEL_TEMPSCEN_RADIOBOX,		OPPD_UI_entries::_onTempScen)
   EVT_RADIOBOX		(FUEL_FVMETHOD_RADIOBOX,		OPPD_UI_entries::_onFVMethod)
   EVT_RADIOBOX		(FUEL_FLAMESCEN_RADIOBOX,		OPPD_UI_entries::_onFlameScen)
   EVT_RADIOBOX		(FUEL_DETACT_RADIOBOX,			OPPD_UI_entries::_onDetAct)
   EVT_BUTTON		(FUEL_KILLEXCEL_BUTTON,			OPPD_UI_entries::_onKillExcel)
   EVT_BUTTON		(FUEL_RESETSHEET_BUTTON,		OPPD_UI_entries::_onResetSheet)
   EVT_COMBOBOX		(FUEL_FUELSEL_COMBOBOX,			OPPD_UI_entries::_onFuelSelect)
   EVT_COMBOBOX		(FUEL_MATSEL_COMBOBOX,			OPPD_UI_entries::_onMatSelect)
   EVT_COMBOBOX		(FUEL_VISMATSEL_COMBOBOX,		OPPD_UI_entries::_onVismatSelect)
   EVT_COMBOBOX		(FUEL_DURMATSEL_COMBOBOX,		OPPD_UI_entries::_onDurmatSelect)
   EVT_COMBOBOX		(FUEL_VISPROPSEL_COMBOBOX,		OPPD_UI_entries::_onVispropSelect)
   EVT_COMBOBOX		(FUEL_VISCOMBSEL_COMBOBOX,		OPPD_UI_entries::_onViscombSelect)
   EVT_COMBOBOX		(FUEL_DETRTISEL_COMBOBOX,		OPPD_UI_entries::_onDetrtiSelect)
   EVT_COMBOBOX		(FUEL_DETTEMPRATSEL_COMBOBOX,	OPPD_UI_entries::_onDettempratSelect)
   EVT_RADIOBOX		(FUEL_DETACTTEMP_RADIOBOX,		OPPD_UI_entries::_onDetActTemp)
   EVT_COMBOBOX		(FUEL_DETSPACESEL_COMBOBOX,		OPPD_UI_entries::_onDetSpaceSelect)
   EVT_COMBOBOX		(FUEL_CABLESEL_COMBOBOX,		OPPD_UI_entries::_onCableSelect)
   //EVT_BUTTON		(FUEL_UPDATE_BUTTON,			OPPD_UI_entries::_onUpdate)
END_EVENT_TABLE()


OPPD_UI_entries::OPPD_UI_entries(wxNotebook* parent)
:wxPanel(parent)
{
	Dialog = ((OPPD_UI_Dialog*)((OPPD_UI_tabs*)GetParent())->GetParent());
	_tempscenRBox = 0;
    _fvmethodRBox = 0;
    _flamescenRBox = 0;
    _detactRBox = 0;
	_detacttemRBox = 0;
	noULlistings = 8;

	//_parent = parent;

	_createComboArrays();
	_buildPage();
}

OPPD_UI_entries::~OPPD_UI_entries()
{
	//delete Dialog;
}


void OPPD_UI_entries::_buildPage()
{
	wxStaticBox* scenarioBox = new wxStaticBox(this, -1, "ESTABLISH THE CALCULATION SCENARIOS", wxDefaultPosition,wxDefaultSize,wxCAPTION);
   wxStaticBox* detectBox = new wxStaticBox(this, -1, "DETECTOR ACTIVATION PARAMETERS");
   wxStaticBox* detectFTHBox = new wxStaticBox(this, -1, "FTH DETECTOR SETTINGS");

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
   _fuelsel       = new wxComboBox(this,FUEL_FUELSEL_COMBOBOX , wxT("Select Fuel"),wxDefaultPosition, wxDefaultSize,20,fuels, wxCB_DROPDOWN);
   _spillvol      = new wxTextCtrl(this, -1, wxT("5.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _spillarea     = new wxTextCtrl(this, -1, wxT("8.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _comwidth      = new wxTextCtrl(this, -1, wxT("40"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _comlength     = new wxTextCtrl(this, -1, wxT("40"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _comheight     = new wxTextCtrl(this, -1, wxT("12"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _matsel        = new wxComboBox(this,FUEL_MATSEL_COMBOBOX , wxT("Select Material"),wxDefaultPosition, wxDefaultSize,16,materials, wxCB_DROPDOWN);
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
   _vismatsel     = new wxComboBox(this,FUEL_VISMATSEL_COMBOBOX , wxT("Select Solid Fuel"),wxDefaultPosition, wxDefaultSize,24,vismats, wxCB_DROPDOWN);
   _durmatsel     = new wxComboBox(this,FUEL_DURMATSEL_COMBOBOX , wxT("Select Solid Fuel"),wxDefaultPosition, wxDefaultSize,22,durmats, wxCB_DROPDOWN);
   _vispropsel    = new wxComboBox(this,FUEL_VISPROPSEL_COMBOBOX , wxT("Select Situation"),wxDefaultPosition, wxDefaultSize,3,visprops, wxCB_DROPDOWN);
   _viscombsel    = new wxComboBox(this,FUEL_VISCOMBSEL_COMBOBOX , wxT("Select Mode of Combustion"),wxDefaultPosition, wxDefaultSize,2,viscomb, wxCB_DROPDOWN);
   _detrtisel     = new wxComboBox(this,FUEL_DETRTISEL_COMBOBOX , wxT("Select Sprinkler Type"),wxDefaultPosition, wxDefaultSize,4,detrti, wxCB_DROPDOWN);
   _dettempratsel = new wxComboBox(this,FUEL_DETTEMPRATSEL_COMBOBOX , wxT("Select Temperature Classification"),wxDefaultPosition, wxDefaultSize,7,dettemprat, wxCB_DROPDOWN);
   _detdistoceil  = new wxTextCtrl(this, -1, wxT("10.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _detraddistospr= new wxTextCtrl(this, -1, wxT("10.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _detceilheight = new wxTextCtrl(this, -1, wxT("20.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _detspacesel   = new wxComboBox(this,FUEL_DETSPACESEL_COMBOBOX , wxT("Select Detector Spacing"),wxDefaultPosition, wxDefaultSize,8,detspace, wxCB_DROPDOWN);
   _cblburnarea   = new wxTextCtrl(this, -1, wxT("10.00"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _cablesel      = new wxComboBox(this,FUEL_CABLESEL_COMBOBOX , wxT("Select Cable Type"),wxDefaultPosition, wxDefaultSize,20,cabletype, wxCB_DROPDOWN);
   
   //build the labels
   wxStaticText* fuelselLabel       = new wxStaticText(this, -1, wxT("Fuel Selection "));
   wxStaticText* spillvolLabel      = new wxStaticText(this, -1, wxT("Fuel Spill Volume (gal)"));
   wxStaticText* spillareaLabel     = new wxStaticText(this, -1, wxT("Fuel Spill Area (ft2)"));
   wxStaticText* comwidthLabel      = new wxStaticText(this, -1, wxT("Compartment Width (ft)"));
   wxStaticText* comlengthLabel     = new wxStaticText(this, -1, wxT("Compartment Length (ft)"));
   wxStaticText* comheightLabel     = new wxStaticText(this, -1, wxT("Compartment Height (ft)"));
   wxStaticText* matselLabel        = new wxStaticText(this, -1, wxT("Material Selection (ft)"));
   wxStaticText* intlinthickLabel   = new wxStaticText(this, -1, wxT("Interior Lining Thickness (in)"));
   wxStaticText* airtempLabel       = new wxStaticText(this, -1, wxT("Air Temperature (F)"));
   wxStaticText* spheatairLabel     = new wxStaticText(this, -1, wxT("Specific Heat Air (kJ/kg*K)"));
   wxStaticText* airdensityLabel    = new wxStaticText(this, -1, wxT("Air Density kg/m3"));
   wxStaticText* massfuelburnLabel  = new wxStaticText(this, -1, wxT("Mass of Solid Fuel Burn (lb) "));
   wxStaticText* solidfuelareaLabel = new wxStaticText(this, -1, wxT("Surface Area of Solid Fuel (sq ft) "));
   wxStaticText* vismatselLabel     = new wxStaticText(this, -1, wxT("Solid Fuel Selection(Particulate Yield Table) "));
   wxStaticText* durmatselLabel     = new wxStaticText(this, -1, wxT("Solid Fuel Selection(HRR,Heat of Comb. Table) "));
   wxStaticText* vispropselLabel    = new wxStaticText(this, -1, wxT("Light Situation Selection "));
   wxStaticText* viscombselLabel    = new wxStaticText(this, -1, wxT("Combustion Mode Selection "));
   wxStaticText* evalabovefireLabel = new wxStaticText(this, -1, wxT("Evaluation Above Fire Source (ft)"));
   wxStaticText* ventwidthLabel     = new wxStaticText(this, -1, wxT("Vent Width(if natural ventilation) (ft)"));
   wxStaticText* ventheightLabel    = new wxStaticText(this, -1, wxT("Vent Height(if natural ventilation) (ft)"));
   wxStaticText* ventdisfloorLabel  = new wxStaticText(this, -1, wxT("Top of Vent from Floor (ft)"));
   wxStaticText* frcvntflwrtLabel   = new wxStaticText(this, -1, wxT("Forced Ventilation Flow Rate(if used)(cfm)"));
   wxStaticText* timeaftignLabel    = new wxStaticText(this, -1, wxT("Time After Ignition (sec)"));
   wxStaticText* detrtiselLabel     = new wxStaticText(this, -1, wxT("Sprinkler Type Selection "));
   wxStaticText* dettempratselLabel = new wxStaticText(this, -1, wxT("Temperature Classification Selection "));
   wxStaticText* dtdstoceilLabel    = new wxStaticText(this, -1, wxT("Dist from Top of Fuel Package to Ceiling (ft)"));
   wxStaticText* dtrdsttosprLabel   = new wxStaticText(this, -1, wxT("Radial Dist from Plume Centerline to Sprinkler/Detector (ft)"));
   wxStaticText* detceilheightLabel = new wxStaticText(this, -1, wxT("Ceiling Height (ft)"));
   wxStaticText* detspaceselLabel   = new wxStaticText(this, -1, wxT("Detector Spacing Selection "));
   wxStaticText* cblburnareaLabel   = new wxStaticText(this, -1, wxT("Exposed Cable Tray Burning Area (ft2)"));
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

   wxStaticBoxSizer* detectFTHPanelGroup = new wxStaticBoxSizer(detectFTHBox, wxVERTICAL);
   detectFTHPanelGroup->Add(_detacttemRBox,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectFTHPanelGroup->Add(detect6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   wxStaticBoxSizer* detectPanelGroup = new wxStaticBoxSizer(detectBox, wxVERTICAL);
   detectPanelGroup->Add(detect1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect3Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detect5Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   detectPanelGroup->Add(detectFTHPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	_detacttemRBox->Show(true);

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

   wxBoxSizer* scenGroup = new wxBoxSizer(wxVERTICAL);
	scenGroup->Add(scenradioGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   scenGroup->Add(scenbuttonGroup,0,wxALIGN_CENTER_HORIZONTAL);

   wxStaticBoxSizer* scenarioGroup = new wxStaticBoxSizer(scenarioBox,wxVERTICAL);
	scenarioGroup->Add(scenGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

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

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(pageLayout); 
}
void OPPD_UI_entries::_createComboArrays()
{
	//The fuel combo box strings
	fuels[0] = ("Methanol");
	fuels[1] = ("Ethanol");
	fuels[2] = ("Butane");
	fuels[3] = ("Benzene");
	fuels[4] = ("Hexane");
	fuels[5] = ("Heptane");
	fuels[6] = ("Xylene");
	fuels[7] = ("Acetone");
	fuels[8] = ("Dioxane");
	fuels[9] = ("Diethy Ether");
	fuels[10] = ("Benzine");
	fuels[11] = ("Gasoline");
	fuels[12] = ("Kerosine");
	fuels[13] = ("Diesel");
	fuels[14] = ("JP-4");
	fuels[15] = ("JP-5");
	fuels[16] = ("Transformer Oil, Hydrocarbon");
	fuels[17] = ("Fuel Oil, Heavy");
	fuels[18] = ("Crude Oil");
	fuels[19] = ("Lube Oil");

	//The Materials Combo Box Strings
	materials[0] = ("Aluminum (pure)");
	materials[1] = ("Steel (0.5% Carbon)");
	materials[2] = ("Concrete");
	materials[3] = ("Brick");
	materials[4] = ("Glass, Plate");
	materials[5] = ("Brick/Concrete Block");
	materials[6] = ("Gypsum Board");
	materials[7] = ("Plywood");
	materials[8] = ("Fiber Insulation Board");
	materials[9] = ("Chipboard");
	materials[10] = ("Aerated Concrete");
	materials[11] = ("Plasterboard");
	materials[12] = ("Calcium Silicate Board");
	materials[13] = ("Alumina Silicate Block");
	materials[14] = ("Glass Fiber Insulation");
	materials[15] = ("Expanded Polystyrene");

	//The Visibility Combo Box Strings
	vismats[0] = ("Wood (Red Oak)");
	vismats[1] = ("Wood (Douglas Fir)");
	vismats[2] = ("Wood (Hemlock)");
	vismats[3] = ("Fiberboard");
	vismats[4] = ("Wool 100%");
	vismats[5] = ("Acrylonitrile-Butadiene-Styrene (ABS)");
	vismats[6] = ("Polymethylemethacrylate (PMMA; PlexiglasTM)");
	vismats[7] = ("Polypropylene");
	vismats[8] = ("Polystyrene");
	vismats[9] = ("Silicone");
	vismats[10] = ("Polyester");
	vismats[11] = ("Nylon");
	vismats[12] = ("Silicone Rubber");
	vismats[13] = ("Polyurethane Foam (Flexible)");
	vismats[14] = ("Polyurethane Foam (Rigid)");
	vismats[15] = ("Polystyrene Foam");
	vismats[16] = ("Polyethylene Foam");
	vismats[17] = ("Phenolic Foam");
	vismats[18] = ("Polyethylene (PE)");
	vismats[19] = ("Polyvinylchloride (PVC)");
	vismats[20] = ("Ethylenetetrafluoroethylene (ETFE; TefzelTM)");
	vismats[21] = ("Perfluoroalkoxy (PFA; TeflonTM)");
	vismats[22] = ("Fluorinated Polyethylene-Polypropylene (FEP; TeflonTM)");
	vismats[23] = ("Tetrafluoroethylene (TFE; TeflonTM)");

	//The Burning Duration Materials Combo Box Strings
	durmats[0] = ("PE/PVC");	
	durmats[1] = ("XPE/FRXPE");	
	durmats[2] = ("XPE/Neoprene");	
	durmats[3] = ("PE, Nylon/PVC, Nylon");	
	durmats[4] = ("Teflon");
	durmats[5] = ("Douglas fir plywood");	
	durmats[6] = ("Fire retardant treated plywood");	
	durmats[7] = ("Particleboard, 19 mm thick");	
	durmats[8] = ("Nylon 6/6");	
	durmats[9] = ("Polymethlmethacrylate (PMMA)");	
	durmats[10] = ("Polypropylene (PP)");	
	durmats[11] = ("Polystyrene (PS)");
	durmats[12] = ("Polyethylene (PE)");	
	durmats[13] = ("Polycarbonate");	
	durmats[14] = ("Polyurethane");	
	durmats[15] = ("Polyvinyl Chloride (PVC) Flexible");	
	durmats[16] = ("Strene-butadiene Copolymers (SBR)");	
	durmats[17] = ("Ethylene Propylene Dien Rubber (EPDM)");	
	durmats[18] = ("Empty Cartons 15 ft high");	
	durmats[19] = ("Wood pallets, stacked 1.5 ft high");	
	durmats[20] = ("Wood pallets, stacked 5 ft high");	
	durmats[21] = ("Wood pallets, stacked 10 ft high");	

	//The Visibility Through Smoke Properties Combo Box Strings
	visprops[0] = ("Illuminated Signs");
	visprops[1] = ("Reflecting Signs");
	visprops[2] = ("Building Components in Reflected Light");

	//The Visibility Combustion Combo Box Strings
	viscomb[0] = ("Smoldering Combustion");
	viscomb[1] = ("Flaming Combustion");

	//The Detection RTI Combo Box Strings
	detrti[0] = ("Standard response bulb");
	detrti[1] = ("Standard response link");
	detrti[2] = ("Quick response bulb");
	detrti[3] = ("Quick response link");

	//The Detector Temperature Rating Combo Box Strings
	dettemprat[0] = ("Ordinary");
	dettemprat[1] = ("Intermediate");
	dettemprat[2] = ("High");
	dettemprat[3] = ("Extra high");
	dettemprat[4] = ("Very extra high");
	dettemprat[5] = ("Ultra high");
	dettemprat[6] = ("Ultra high");

	//The Detector Spacing Combo Box Strings
	detspace[0] = ("10 ft");
	detspace[1] = ("15 ft");
	detspace[2] = ("20 ft");
	detspace[3] = ("25 ft");
	detspace[4] = ("30 ft");
	detspace[5] = ("40 ft");
	detspace[6] = ("50 ft");
	detspace[7] = ("70 ft");

	//The Cable Type Combo Box Strings
	cabletype[0] = ("ld PE 1071(kW/m2)");
	cabletype[1] = ("PE/PVC 589(kW/m2)");
	cabletype[2] = ("XPE/FRXPE 475(kW/m2)");
	cabletype[3] = ("PE/PVC 395(kW/m2)");
	cabletype[4] = ("PE/PVC 359(kW/m2)");
	cabletype[5] = ("XPE/Neoprene 354(kW/m2)");
	cabletype[6] = ("PE, PP/Cl.S.PE 345(kW/m2)");
	cabletype[7] = ("PE/PVC 312(kW/m2)");
	cabletype[8] = ("XPE/Neoprene 302(kW/m2)");
	cabletype[9] = ("PE, PP/Cl.S.PE 299(kW/m2)");
	cabletype[10] = ("PE, PP/Cl.S.PE 271(kW/m2)");
	cabletype[11] = ("FRXPE/Cl.S.PE 258(kW/m2)");
	cabletype[12] = ("PE, Nylon/PVC, Nylon 231(kW/m2)");
	cabletype[13] = ("PE, Nylon/PVC, Nylon 218(kW/m2)");
	cabletype[14] = ("XPE/Cl.S.PE 204(kW/m2)");
	cabletype[15] = ("Silicone, glass braid, asbestos 182(kW/m2)");
	cabletype[16] = ("XPE/XPE 178(kW/m2)");
	cabletype[17] = ("PE, PP/Cl.S.PE 177(kW/m2)");
	cabletype[18] = ("Silicone, glass braid 128(kW/m2)");
	cabletype[19] = ("Teflon 98(kW/m2)");




}


void OPPD_UI_entries::_onKillExcel(wxCommandEvent& event)
{
	(*(Dialog->p_killexcel)) = 1;
}

void OPPD_UI_entries::_onResetSheet(wxCommandEvent& event)
{

}

void OPPD_UI_entries::_onTempScen(wxCommandEvent& event)
{
	if (_tempscenRBox->GetSelection() == 1 || _tempscenRBox->GetSelection() == 2)
		_fvmethodRBox->Enable( true );
	else
		_fvmethodRBox->Enable( false );
}

void OPPD_UI_entries::_onFVMethod(wxCommandEvent& event)
{
}

void OPPD_UI_entries::_onFlameScen(wxCommandEvent& event)
{
}

void OPPD_UI_entries::_onDetAct(wxCommandEvent& event)
{
}


void OPPD_UI_entries::_onDetActTemp(wxCommandEvent& event)
{
	(*(Dialog->p_detacttemp)) = _detacttemRBox->GetSelection();

	if ( (*(Dialog->p_detacttemp)) < 3)
		noULlistings = 8;
	else if ((*(Dialog->p_detacttemp)) == 3)
		noULlistings = 6;
	else if ((*(Dialog->p_detacttemp)) == 4)
		noULlistings = 5;
	else if ((*(Dialog->p_detacttemp)) == 5)
		noULlistings = 3;

	_detspacesel->Clear();
	for(int i=0;i<noULlistings;i++)
			_detspacesel->Append(detspace[i]);
}
void OPPD_UI_entries::_onFuelSelect(wxCommandEvent& event)
{
	for(int i=0;i<20;i++)
	{
		if( _fuelsel->GetStringSelection() == fuels[i])
			(*(Dialog->p_fuelselindex)) = i+1;
	}
}


void OPPD_UI_entries::_onMatSelect(wxCommandEvent& event)
{
	for(int i=0;i<16;i++)
	{
		if( _matsel->GetStringSelection() == materials[i])
			(*(Dialog->p_matselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onVismatSelect(wxCommandEvent& event)
{
	for(int i=0;i<24;i++)
	{
		if( _vismatsel->GetStringSelection() == vismats[i])
			(*(Dialog->p_vismatselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onDurmatSelect(wxCommandEvent& event)
{
	for(int i=0;i<22;i++)
	{
		if( _durmatsel->GetStringSelection() == durmats[i])
			(*(Dialog->p_durmatselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onVispropSelect(wxCommandEvent& event)
{
	for(int i=0;i<3;i++)
	{
		if( _vispropsel->GetStringSelection() == visprops[i])
			(*(Dialog->p_vispropselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onViscombSelect(wxCommandEvent& event)
{
	for(int i=0;i<2;i++)
	{
		if( _viscombsel->GetStringSelection() == viscomb[i])
			(*(Dialog->p_viscombselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onDetrtiSelect(wxCommandEvent& event)
{
	for(int i=0;i<4;i++)
	{
		if( _detrtisel->GetStringSelection() == detrti[i])
			(*(Dialog->p_detrtiselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onDettempratSelect(wxCommandEvent& event)
{
	for(int i=0;i<7;i++)
	{
		if( _dettempratsel->GetStringSelection() == dettemprat[i])
			(*(Dialog->p_dettempratselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onDetSpaceSelect(wxCommandEvent& event)
{   
	for(int i=0;i<noULlistings;i++)
	{
		if( _detspacesel->GetStringSelection() == detspace[i])
			(*(Dialog->p_detspaceselindex)) = i+1;
	}
}

void OPPD_UI_entries::_onCableSelect(wxCommandEvent& event)
{
	for(int i=0;i<20;i++)
	{
		if( _cablesel->GetStringSelection() == cabletype[i])
			(*(Dialog->p_cableselindex)) = i+1;
	}
}
void OPPD_UI_entries::_onUpdate(wxCommandEvent& event)
{
	
}