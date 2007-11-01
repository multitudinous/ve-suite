//---------------------------------------------------------------------------
//
// Name:        EphemerisDialog.cpp
// Author:      G-Biv
// Created:     10/3/2007 12:36:53 AM
// Description: EphemerisDialog class implementation
//
//---------------------------------------------------------------------------

#include "EphemerisDialog.h"

//Do not add custom headers
//wxDev-C++ designer will remove them
////Header Include Start
////Header Include End

//----------------------------------------------------------------------------
// EphemerisDialog
//----------------------------------------------------------------------------
//Add Custom Events only in the appropriate block.
//Code added in other places will be removed by wxDev-C++
////Event Table Start
BEGIN_EVENT_TABLE(EphemerisDialog,wxDialog)
	////Manual Code Start
	////Manual Code End
	
	EVT_CLOSE(EphemerisDialog::OnClose)
	EVT_CHOICE(ID_M_AMPM,EphemerisDialog::m_amPmSelected)
	EVT_SPINCTRL(ID_M_LONGITUDEMINUTES,EphemerisDialog::m_longitudeMinutesUpdated)
	
	EVT_TEXT(ID_M_HOUR,EphemerisDialog::m_hourTextUpdated)
	
	EVT_CALENDAR_DAY(ID_M_CALENDAR,EphemerisDialog::m_calendarDay)
END_EVENT_TABLE()
////Event Table End

EphemerisDialog::EphemerisDialog(wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}

EphemerisDialog::~EphemerisDialog()
{
} 

void EphemerisDialog::CreateGUIControls()
{
	//Do not add custom code between
	//GUI Items Creation Start and GUI Items Creation End.
	//wxDev-C++ designer will remove them.
	//Add the custom code before or after the blocks
	////GUI Items Creation Start

	m_mainSizer = new wxBoxSizer(wxVERTICAL);
	this->SetSizer(m_mainSizer);
	//this->SetAutoLayout(true);

	m_dataEntryPages = new wxNotebook(this, ID_M_DATAENTRYPAGES, wxDefaultPosition,wxDefaultSize);
	m_dataEntryPages->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_mainSizer->Add(m_dataEntryPages,1,wxALIGN_CENTER | wxALL,5);

	m_dateTime = new wxPanel(m_dataEntryPages, ID_M_DATETIME, wxDefaultPosition,wxDefaultSize);
	m_dateTime->SetToolTip(wxT("Set the date and time"));
	m_dateTime->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_dataEntryPages->AddPage(m_dateTime, wxT("Date and Time"));

	m_latitudeLongitude = new wxPanel(m_dataEntryPages, ID_M_LATITUDELONGITUDE,wxDefaultPosition,wxDefaultSize);
	m_latitudeLongitude->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_dataEntryPages->AddPage(m_latitudeLongitude, wxT("Latitude and Longitude"));

	m_dateTimeSizer = new wxBoxSizer(wxHORIZONTAL);
	m_dateTime->SetSizer(m_dateTimeSizer);
	m_dateTime->SetAutoLayout(true);

	wxStaticBox* m_dateSizer_StaticBoxObj = new wxStaticBox(m_dateTime, wxID_ANY, wxT("Date"));
	m_dateSizer = new wxStaticBoxSizer(m_dateSizer_StaticBoxObj, wxHORIZONTAL);
	m_dateTimeSizer->Add(m_dateSizer, 1, wxALIGN_CENTER |wxEXPAND| wxALL, 5);

	m_calendar = new wxCalendarCtrl(m_dateTime, ID_M_CALENDAR, wxDateTime(3,(wxDateTime::Month)10,2007),
                                        wxDefaultPosition, wxDefaultSize,
                                         wxCAL_SUNDAY_FIRST | wxCAL_SHOW_HOLIDAYS | wxCAL_SHOW_SURROUNDING_WEEKS | wxCAL_SEQUENTIAL_MONTH_SELECTION);
	m_calendar->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_dateSizer->Add(m_calendar,1,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	wxStaticBox* m_timeSizer_StaticBoxObj = new wxStaticBox(m_dateTime, wxID_ANY, wxT("Time"));
	m_timeSizer = new wxStaticBoxSizer(m_timeSizer_StaticBoxObj, wxHORIZONTAL);
	m_dateTimeSizer->Add(m_timeSizer, 1, wxALIGN_CENTER | wxALL, 5);

	m_hour = new wxSpinCtrl(m_dateTime, ID_M_HOUR, wxT("12"), wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS|wxSP_WRAP, 1, 12, 12);
	m_hour->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_timeSizer->Add(m_hour,2,wxALIGN_CENTER |wxEXPAND| wxALL,5);

	m_hourColon = new wxStaticText(m_dateTime, ID_M_HOURCOLON, wxT(":"), wxDefaultPosition, wxDefaultSize, 0);
	m_hourColon->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_timeSizer->Add(m_hourColon,1,wxALIGN_CENTER |wxEXPAND| wxALL,5);

	m_minutes = new wxSpinCtrl(m_dateTime, ID_M_MINUTES, wxT("0"), wxDefaultPosition, wxDefaultSize,wxSP_ARROW_KEYS|wxSP_WRAP, 0, 59, 0);
	m_minutes->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_timeSizer->Add(m_minutes,2,wxALIGN_CENTER |wxEXPAND| wxALL,5);

	m_latLongSizer = new wxBoxSizer(wxVERTICAL);
	m_latitudeLongitude->SetSizer(m_latLongSizer);
	m_latitudeLongitude->SetAutoLayout(true);

	wxStaticBox* m_latitudeSizer_StaticBoxObj = new wxStaticBox(m_latitudeLongitude, wxID_ANY, wxT("Latitude"));
	m_latitudeSizer = new wxStaticBoxSizer(m_latitudeSizer_StaticBoxObj, wxHORIZONTAL);
	m_latLongSizer->Add(m_latitudeSizer, 1, wxALIGN_CENTER | wxALL, 5);

	m_latDegrees = new wxSpinCtrl(m_latitudeLongitude, ID_M_LATDEGREES, wxT("0"), wxDefaultPosition, wxDefaultSize, 
                                      wxSP_ARROW_KEYS|wxSP_WRAP, 0, 90, 0);
	m_latDegrees->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_latitudeSizer->Add(m_latDegrees,2,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	m_degreeSymbol = new wxStaticText(m_latitudeLongitude, ID_M_DEGREESYMBOL, wxT("o"), wxDefaultPosition, wxDefaultSize);
	m_degreeSymbol->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_latitudeSizer->Add(m_degreeSymbol,1,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	m_latitudeMinutes = new wxSpinCtrl(m_latitudeLongitude, ID_M_LATITUDEMINUTES, wxT("0"),
                                           wxDefaultPosition, wxDefaultSize,wxSP_WRAP|wxSP_ARROW_KEYS,
                                           0, 60, 0);
	m_latitudeMinutes->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_latitudeSizer->Add(m_latitudeMinutes,2,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	m_minutesSymbol = new wxStaticText(m_latitudeLongitude, ID_M_MINUTESSYMBOL, wxT("\""),
                                           wxDefaultPosition, wxDefaultSize);
	m_minutesSymbol->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_latitudeSizer->Add(m_minutesSymbol,1,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	/* North
	South
	*/
	wxArrayString arrayStringFor_m_latHemisphere;
	arrayStringFor_m_latHemisphere.Add(wxT("North"));
	arrayStringFor_m_latHemisphere.Add(wxT("South"));
	m_latHemisphere = new wxChoice(m_latitudeLongitude, ID_M_LATHEMISPHERE,
                                       wxDefaultPosition, wxDefaultSize,
                                       arrayStringFor_m_latHemisphere);
	m_latHemisphere->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_latHemisphere->SetSelection(0);
	m_latitudeSizer->Add(m_latHemisphere,2,wxALIGN_CENTER |wxEXPAND| wxALL,5);

	wxStaticBox* m_longitudeSizer_StaticBoxObj = new wxStaticBox(m_latitudeLongitude, wxID_ANY, wxT("Longitude"));
	m_longitudeSizer = new wxStaticBoxSizer(m_longitudeSizer_StaticBoxObj, wxHORIZONTAL);
	m_latLongSizer->Add(m_longitudeSizer, 1, wxALIGN_CENTER | wxALL, 5);

	m_longitudeDegree = new wxSpinCtrl(m_latitudeLongitude, ID_M_LONGITUDEDEGREE, wxT("0"),
                                           wxDefaultPosition, wxDefaultSize,
                                           wxSP_WRAP, 0, 180, 0);
	m_longitudeDegree->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_longitudeSizer->Add(m_longitudeDegree,2,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	m_degreeLonSymbol = new wxStaticText(m_latitudeLongitude, ID_M_DEGREELONSYMBOL, wxT("o"),
                                             wxDefaultPosition, wxDefaultSize);
	m_degreeLonSymbol->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_longitudeSizer->Add(m_degreeLonSymbol,1,wxALIGN_CENTER | wxALL,5);

	m_longitudeMinutes = new wxSpinCtrl(m_latitudeLongitude, ID_M_LONGITUDEMINUTES, wxT("0"),
                                            wxDefaultPosition, wxDefaultSize,
                                            wxSP_WRAP, 0, 60, 0);
	m_longitudeMinutes->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_longitudeSizer->Add(m_longitudeMinutes,2,wxALIGN_CENTER | wxALL,5);

	m_lonMinutesSymbol = new wxStaticText(m_latitudeLongitude, ID_M_LONMINUTESSYMBOL, wxT("\""),
                                              wxDefaultPosition, wxDefaultSize);

	m_lonMinutesSymbol->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_longitudeSizer->Add(m_lonMinutesSymbol,1,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	wxArrayString arrayStringFor_m_lonHemisphere;
	arrayStringFor_m_lonHemisphere.Add(wxT("East"));
	arrayStringFor_m_lonHemisphere.Add(wxT("West"));
	m_lonHemisphere = new wxChoice(m_latitudeLongitude, ID_M_LONHEMISPHERE,
                                       wxDefaultPosition, wxDefaultSize,
                                       arrayStringFor_m_lonHemisphere);
	m_lonHemisphere->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_lonHemisphere->SetSelection(0);
	m_longitudeSizer->Add(m_lonHemisphere,2,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	m_buttonsSizer = new wxBoxSizer(wxHORIZONTAL);
	m_mainSizer->Add(m_buttonsSizer, 1, wxALIGN_CENTER | wxALL, 5);

	m_cancel = new wxButton(this, wxID_CANCEL, wxT("Cancel"),
                               wxDefaultPosition, wxDefaultSize);
	m_cancel->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_buttonsSizer->Add(m_cancel,0,wxALIGN_CENTER | wxALL,5);

	m_close = new wxButton(this, wxID_OK, wxT("OK"),
                               wxDefaultPosition, wxDefaultSize);
	m_close->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_buttonsSizer->Add(m_close,0,wxALIGN_CENTER | wxALL,5);

	wxArrayString arrayStringFor_m_amPm;
	arrayStringFor_m_amPm.Add(wxT("AM"));
	arrayStringFor_m_amPm.Add(wxT("PM"));
	m_amPm = new wxChoice(m_dateTime, ID_M_AMPM,
                              wxDefaultPosition, wxDefaultSize,
                              arrayStringFor_m_amPm);
	m_amPm->SetToolTip(wxT("AM/PM"));
	m_amPm->SetFont(wxFont(9, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Segoe UI")));
	m_amPm->SetSelection(0);
	m_timeSizer->Add(m_amPm,2,wxALIGN_CENTER | wxEXPAND|wxALL,5);

	SetTitle(wxT("Ephemeris Data"));
	SetIcon(wxNullIcon);
	
	GetSizer()->Layout();
	GetSizer()->Fit(this);
	GetSizer()->SetSizeHints(this);
	Center();
        SetAutoLayout(true);
	
	////GUI Items Creation End
}

void EphemerisDialog::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}



/*
 * m_longitudeMinutesUpdated
 */
void EphemerisDialog::m_longitudeMinutesUpdated(wxSpinEvent& event )
{
	// insert your code here
}

/*
 * m_amPmSelected
 */
void EphemerisDialog::m_amPmSelected(wxCommandEvent& event )
{
	// insert your code here
}

/*
 * m_calendarDay
 */
void EphemerisDialog::m_calendarDay(wxCalendarEvent& event)
{
	// insert your code here
}

/*
 * m_hourTextUpdated
 */
void EphemerisDialog::m_hourTextUpdated(wxCommandEvent& event )
{
	// insert your code here
}
