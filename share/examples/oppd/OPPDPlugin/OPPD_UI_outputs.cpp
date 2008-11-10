#include "OPPD_UI_outputs.h"


BEGIN_EVENT_TABLE(OPPD_UI_outputs, wxPanel)
   //EVT_RADIOBOX		(FUEL_TEMPSCEN_RADIOBOX,		OPPD_UI_outputs::_onTempScen)
   //EVT_BUTTON		(FUEL_KILLEXCEL_BUTTON,			OPPD_UI_outputs::_onKillExcel)
   //EVT_COMBOBOX		(FUEL_FUELSEL_COMBOBOX,			OPPD_UI_outputs::_onFuelSelect)

END_EVENT_TABLE()


OPPD_UI_outputs::OPPD_UI_outputs(wxNotebook* parent)
:wxPanel(parent)
{
	Dialog = ((OPPD_UI_Dialog*)((OPPD_UI_tabs*)GetParent())->GetParent());

	_buildPage();
}

OPPD_UI_outputs::~OPPD_UI_outputs()
{
	//delete Dialog;
}


void OPPD_UI_outputs::_buildPage()
{
	//*************************THE BURNING_DURATION_SOLIDS INTERFACE
	wxStaticText* tsecLabel = new wxStaticText(this, -1, wxT("Burning Duration Calculation(seconds)"));
	_tsec = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* burdur6Group  = new wxBoxSizer(wxHORIZONTAL);
	burdur6Group->Add(tsecLabel,1,wxALIGN_LEFT|wxEXPAND);
	burdur6Group->Add(_tsec,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticText* tminLabel = new wxStaticText(this, -1, wxT("Burning Duration Calculation(minutes)"));
	_tmin = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* burdur7Group  = new wxBoxSizer(wxHORIZONTAL);
	burdur7Group->Add(tminLabel,1,wxALIGN_LEFT|wxEXPAND);
	burdur7Group->Add(_tmin,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* burdurBox = new wxStaticBox(this, -1, "Burning_Duration_Solid Worksheet");
    wxStaticBoxSizer* burdurPanelGroup = new wxStaticBoxSizer(burdurBox,wxVERTICAL);
	burdurPanelGroup->Add(burdur6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	burdurPanelGroup->Add(burdur7Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);


	//*************************THE CABLE_HRR_CALCULATIONS INTERFACE
	wxStaticText* hrrkwLabel = new wxStaticText(this, -1, wxT("HRR Calculation (kW)"));
	_hrrkw = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* cabhrr4Group  = new wxBoxSizer(wxHORIZONTAL);
	cabhrr4Group->Add(hrrkwLabel,1,wxALIGN_LEFT|wxEXPAND);
	cabhrr4Group->Add(_hrrkw,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticText* hrrbtuLabel = new wxStaticText(this, -1, wxT("HRR Calculation (BTU/sec)"));
	_hrrbtu = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* cabhrr5Group  = new wxBoxSizer(wxHORIZONTAL);
	cabhrr5Group->Add(hrrbtuLabel,1,wxALIGN_LEFT|wxEXPAND);
	cabhrr5Group->Add(_hrrbtu,0,wxALIGN_RIGHT|wxEXPAND);
	
	wxStaticBox* cabhrrBox = new wxStaticBox(this, -1, "Cable_HRR_Calculations Worksheet");
    wxStaticBoxSizer* cabhrrPanelGroup = new wxStaticBoxSizer(cabhrrBox,wxVERTICAL);
	cabhrrPanelGroup->Add(cabhrr4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	cabhrrPanelGroup->Add(cabhrr5Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//*************************THE DETECTOR_ACTIVATION_TIME INTERFACE
	//The Sprinkler Tab
	wxStaticText* detsprinktimeLabel = new wxStaticText(this, -1, wxT("Sprinkler Response Time Calculation (min)"));
	_detsprinktime = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* detsprink9Group  = new wxBoxSizer(wxHORIZONTAL);
	detsprink9Group->Add(detsprinktimeLabel,1,wxALIGN_LEFT|wxEXPAND);
	detsprink9Group->Add(_detsprinktime,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* detsprinkBox = new wxStaticBox(this, -1, "Sprinkler Tab");
    wxStaticBoxSizer* detsprinkPanelGroup = new wxStaticBoxSizer(detsprinkBox,wxVERTICAL);
	detsprinkPanelGroup->Add(detsprink9Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//The Smoke Tab
	wxStaticText* detsmtimeLabel = new wxStaticText(this, -1, wxT("Smoke Detector Response Time Calculation (min)"));
	_detsmtime = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* detsmoke4Group  = new wxBoxSizer(wxHORIZONTAL);
	detsmoke4Group->Add(detsmtimeLabel,1,wxALIGN_LEFT|wxEXPAND);
	detsmoke4Group->Add(_detsmtime,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* detsmokeBox = new wxStaticBox(this, -1, "Smoke Tab");
    wxStaticBoxSizer* detsmokePanelGroup = new wxStaticBoxSizer(detsmokeBox,wxVERTICAL);
	detsmokePanelGroup->Add(detsmoke4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//The FTH Detector Tab
	wxStaticText* detfthtimeLabel = new wxStaticText(this, -1, wxT("Detector Response Time Calculation (min)"));
	_detfthtime = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* detfth7Group  = new wxBoxSizer(wxHORIZONTAL);
	detfth7Group->Add(detfthtimeLabel,1,wxALIGN_LEFT|wxEXPAND);
	detfth7Group->Add(_detfthtime,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* detfthBox = new wxStaticBox(this, -1, "FTH Detector Tab");
    wxStaticBoxSizer* detfthPanelGroup = new wxStaticBoxSizer(detfthBox,wxVERTICAL);
	detfthPanelGroup->Add(detfth7Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	wxStaticBox* detacttimeBox = new wxStaticBox(this, -1, "Detector_Activation_Time Worksheet");
    wxStaticBoxSizer* detacttimePanelGroup = new wxStaticBoxSizer(detacttimeBox,wxVERTICAL);
	detacttimePanelGroup->Add(detsprinkPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	detacttimePanelGroup->Add(detsmokePanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	detacttimePanelGroup->Add(detfthPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//*************************THE FLAME_HEIGHT_CALCULATIONS INTERFACE
	//The Wall_Line_Flame_Height tab
	wxStaticText* flwallinehgtLabel = new wxStaticText(this, -1, wxT("Estimated Wall Line Flame Height (ft)"));
	_flwallinehgt = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* flwalline6Group  = new wxBoxSizer(wxHORIZONTAL);
	flwalline6Group->Add(flwallinehgtLabel,1,wxALIGN_LEFT|wxEXPAND);
	flwalline6Group->Add(_flwallinehgt,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* flwalllineBox = new wxStaticBox(this, -1, "Wall_Line_Flame_Height tab");
    wxStaticBoxSizer* flwallinePanelGroup = new wxStaticBoxSizer(flwalllineBox,wxVERTICAL);
	flwallinePanelGroup->Add(flwalline6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//The Corner_Flame_Height tab
	wxStaticText* flcornerhgtLabel = new wxStaticText(this, -1, wxT("Estimated Corner Fire Flame Height (ft)"));
	_flcornerhgt = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* flcorner6Group  = new wxBoxSizer(wxHORIZONTAL);
	flcorner6Group->Add(flcornerhgtLabel,1,wxALIGN_LEFT|wxEXPAND);
	flcorner6Group->Add(_flcornerhgt,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* flcornerBox = new wxStaticBox(this, -1, "Corner_Flame_Height tab");
    wxStaticBoxSizer* flcornerPanelGroup = new wxStaticBoxSizer(flcornerBox,wxVERTICAL);
	flcornerPanelGroup->Add(flcorner6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//The Wall_Flame_Height tab
	wxStaticText* flwallhgtLabel = new wxStaticText(this, -1, wxT("Estimated Wall Fire Flame Height (ft)"));
	_flwallhgt = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* flwall6Group  = new wxBoxSizer(wxHORIZONTAL);
	flwall6Group->Add(flwallhgtLabel,1,wxALIGN_LEFT|wxEXPAND);
	flwall6Group->Add(_flwallhgt,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* flwallBox = new wxStaticBox(this, -1, "Wall_Flame_Height tab");
    wxStaticBoxSizer* flwallPanelGroup = new wxStaticBoxSizer(flwallBox,wxVERTICAL);
	flwallPanelGroup->Add(flwall6Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	wxStaticBox* flhgtBox = new wxStaticBox(this, -1, "Flame_Height_Calculations Worksheet");
    wxStaticBoxSizer* flhgtPanelGroup = new wxStaticBoxSizer(flhgtBox,wxVERTICAL);
	flhgtPanelGroup->Add(flwallinePanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	flhgtPanelGroup->Add(flcornerPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	flhgtPanelGroup->Add(flwallPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//*************************THE HRR_FLAME_HEIGHT_BURNING_DURATION_CALCULATIONS INTERFACE
	wxStaticText* hrrhrrLabel = new wxStaticText(this, -1, wxT("Estimated Pool Fire HRR (kW)"));
	_hrrhrr = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* hrr1Group  = new wxBoxSizer(wxHORIZONTAL);
	hrr1Group->Add(hrrhrrLabel,1,wxALIGN_LEFT|wxEXPAND);
	hrr1Group->Add(_hrrhrr,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticText* hrrburndurLabel = new wxStaticText(this, -1, wxT("Estimated Pool Fire Burning Duration (min)"));
	_hrrburndur = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* hrr2Group  = new wxBoxSizer(wxHORIZONTAL);
	hrr2Group->Add(hrrburndurLabel,1,wxALIGN_LEFT|wxEXPAND);
	hrr2Group->Add(_hrrburndur,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticText* hrrhgtheskLabel = new wxStaticText(this, -1, wxT("Estimated Pool Fire Flame Height:Heskestad (ft)"));
	_hrrhgthesk = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* hrr3Group  = new wxBoxSizer(wxHORIZONTAL);
	hrr3Group->Add(hrrhgtheskLabel,1,wxALIGN_LEFT|wxEXPAND);
	hrr3Group->Add(_hrrhgthesk,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticText* hrrhgtthomLabel = new wxStaticText(this, -1, wxT("Estimated Pool Fire Flame Height:Thomas (ft)"));
	_hrrhgtthom = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* hrr4Group  = new wxBoxSizer(wxHORIZONTAL);
	hrr4Group->Add(hrrhgtthomLabel,1,wxALIGN_LEFT|wxEXPAND);
	hrr4Group->Add(_hrrhgtthom,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* hrrBox = new wxStaticBox(this, -1, "HRR_Flame_Height_Burning_Duration_Calculations Worksheet");
    wxStaticBoxSizer* hrrPanelGroup = new wxStaticBoxSizer(hrrBox,wxVERTICAL);
	hrrPanelGroup->Add(hrr1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	hrrPanelGroup->Add(hrr2Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	hrrPanelGroup->Add(hrr3Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	hrrPanelGroup->Add(hrr4Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//*************************THE PLUME_TEMPERATURE_CALCULATIONS INTERFACE
	wxStaticText* pltempLabel = new wxStaticText(this, -1, wxT("Estimated Plume Centerline Temp (F)"));
	_pltemp = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* plume1Group  = new wxBoxSizer(wxHORIZONTAL);
	plume1Group->Add(pltempLabel,1,wxALIGN_LEFT|wxEXPAND);
	plume1Group->Add(_pltemp,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* plumeBox = new wxStaticBox(this, -1, "Plume_Temperature_Calculations Worksheet");
    wxStaticBoxSizer* plumePanelGroup = new wxStaticBoxSizer(plumeBox,wxVERTICAL);
	plumePanelGroup->Add(plume1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//*************************THE TEMPERATURE_CLOSED_COMPARTMENT INTERFACE
	wxStaticText* tcltempLabel = new wxStaticText(this, -1, wxT("Compartment Hot Gas Layer Temp: Closed Door (F)"));
	_tcltemp = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* tclosed1Group  = new wxBoxSizer(wxHORIZONTAL);
	tclosed1Group->Add(tcltempLabel,1,wxALIGN_LEFT|wxEXPAND);
	tclosed1Group->Add(_tcltemp,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* tclosedBox = new wxStaticBox(this, -1, "Temperature_Closed_Compartment Worksheet");
    wxStaticBoxSizer* tclosedPanelGroup = new wxStaticBoxSizer(tclosedBox,wxVERTICAL);
	tclosedPanelGroup->Add(tclosed1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	//*************************THE VISIBILITY_THROUGH_SMOKE INTERFACE
	wxStaticText* visdistLabel = new wxStaticText(this, -1, wxT("Visibility Through Smoke Calculation (ft)"));
	_visdist = new wxTextCtrl(this, -1, wxT("no data"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
	wxBoxSizer* vis1Group  = new wxBoxSizer(wxHORIZONTAL);
	vis1Group->Add(visdistLabel,1,wxALIGN_LEFT|wxEXPAND);
	vis1Group->Add(_visdist,0,wxALIGN_RIGHT|wxEXPAND);

	wxStaticBox* visBox = new wxStaticBox(this, -1, "Visibility_Through_Smoke Worksheet");
    wxStaticBoxSizer* visPanelGroup = new wxStaticBoxSizer(visBox,wxVERTICAL);
	visPanelGroup->Add(vis1Group,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);


	_loadresultsButton	    = new wxButton(this,wxID_APPLY,wxT("Update Fire Scenario Results"));


	wxBoxSizer* Row1 = new wxBoxSizer(wxVERTICAL);
    Row1->Add(burdurPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Row1->Add(cabhrrPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Row1->Add(flhgtPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Row1->Add(tclosedPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Row1->Add(_loadresultsButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	wxBoxSizer* Row2 = new wxBoxSizer(wxVERTICAL);
    Row2->Add(detacttimePanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Row2->Add(hrrPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Row2->Add(plumePanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
	Row2->Add(visPanelGroup,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

	wxBoxSizer* pageLayout = new wxBoxSizer(wxHORIZONTAL);
    pageLayout->Add(Row1,0,wxALIGN_CENTER_HORIZONTAL);
	pageLayout->Add(Row2,0,wxALIGN_CENTER_HORIZONTAL);

	pageLayout->Layout();
		//set this flag and let wx handle alignment
	SetAutoLayout(true);
	//assign the group to the panel
	SetSizer(pageLayout);
	pageLayout->Fit(this);  

}