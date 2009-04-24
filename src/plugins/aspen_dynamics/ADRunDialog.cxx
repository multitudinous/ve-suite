#include "ADRunDialog.h"

BEGIN_EVENT_TABLE(ADRunDialog,wxDialog)
    EVT_CLOSE(ADRunDialog::OnClose)
	EVT_COMBOBOX(ID_MMODE,ADRunDialog::mModeSelected)
	EVT_BUTTON(ID_CANCELBUTTON,ADRunDialog::cancelButtonClick)
	EVT_BUTTON(ID_OKBUTTON,ADRunDialog::okButtonClick)
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ADRunDialog::ADRunDialog(wxWindow *parent, wxWindowID id,
                         const wxString &title, const wxPoint &position,
                         const wxSize& size, long style)
: wxDialog(parent, id, title, position, size, style)
{
	CreateGUIControls();
}
////////////////////////////////////////////////////////////////////////////////
ADRunDialog::~ADRunDialog()
{
} 
////////////////////////////////////////////////////////////////////////////////
void ADRunDialog::CreateGUIControls()
{
	SetTitle(wxT("Run Options"));
	SetIcon(wxNullIcon);
	SetSize(8,8,316,522);
	Center();
	

	wxArrayString arrayStringFor_mMode;
	arrayStringFor_mMode.Add(wxT("Steady State"));
	arrayStringFor_mMode.Add(wxT("Optimization"));
	arrayStringFor_mMode.Add(wxT("Estimation"));
	arrayStringFor_mMode.Add(wxT("Dynamic"));
	arrayStringFor_mMode.Add(wxT("Initialization"));
	mMode = new wxComboBox(this, ID_MMODE, wxT(""), wxPoint(164,28),
        wxSize(126,21), arrayStringFor_mMode, wxCB_READONLY,
        wxDefaultValidator, wxT("mMode"));
	mMode->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mPauseAt = new wxCheckBox(this, ID_MPAUSEAT, wxT("Pause at"),
        wxPoint(13,312), wxSize(97,17), 0, wxDefaultValidator,
        wxT("mPauseAt"));
	mPauseAt->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mHistory = new wxCheckBox(this, ID_MHISTORY,
        wxT("Record history for all variables"), wxPoint(13,393),
        wxSize(172,17), 0, wxDefaultValidator, wxT("mHistory"));
	mHistory->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mRTS = new wxCheckBox(this, ID_MRTS, wxT("Real time synchronization"),
        wxPoint(13,366), wxSize(145,17), 0, wxDefaultValidator, wxT("mRTS"));
	mRTS->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false, wxT("Tahoma")));

	mPauseAfter = new wxCheckBox(this, ID_MPAUSEAFTER, wxT("Pause after"),
        wxPoint(13,339), wxSize(97,17), 0, wxDefaultValidator,
        wxT("mPauseAfter"));
	mPauseAfter->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	wxArrayString arrayStringFor_mSynchro;
	arrayStringFor_mSynchro.Add(wxT("Low"));
	arrayStringFor_mSynchro.Add(wxT("Medium"));
	arrayStringFor_mSynchro.Add(wxT("High"));
	arrayStringFor_mSynchro.Add(wxT("Full"));
	mSynchro = new wxComboBox(this, ID_MSYNCHRO, wxT(""), wxPoint(160,417),
        wxSize(80,21), arrayStringFor_mSynchro, wxCB_READONLY, 
        wxDefaultValidator, wxT("mSynchro"));
	mSynchro->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText15 = new wxStaticText(this, ID_WXSTATICTEXT15,
        wxT("interface should display time:"), wxPoint(15,248), wxDefaultSize,
        0, wxT("WxStaticText15"));
	WxStaticText15->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText14 = new wxStaticText(this, ID_WXSTATICTEXT14,
        wxT("Select the time units in which the user"), wxPoint(15,234),
        wxDefaultSize, 0, wxT("WxStaticText14"));
	WxStaticText14->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText13 = new wxStaticText(this, ID_WXSTATICTEXT13, wxT("Factor"),
        wxPoint(247,372), wxDefaultSize, 0, wxT("WxStaticText13"));
	WxStaticText13->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText12 = new wxStaticText(this, ID_WXSTATICTEXT12,
        wxT("Intervals"), wxPoint(247,342), wxDefaultSize, 0,
        wxT("WxStaticText12"));
	WxStaticText12->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText11 = new wxStaticText(this, ID_WXSTATICTEXT11, wxT("Hours"),
        wxPoint(247,314), wxDefaultSize, 0, wxT("WxStaticText11"));
	WxStaticText11->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mRTSFactor = new wxTextCtrl(this, ID_MRTSFACTOR, wxT("0"),
        wxPoint(160,366), wxSize(80,19), 0, wxDefaultValidator,
        wxT("mRTSFactor"));
	mRTSFactor->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mPauseAfterTime = new wxTextCtrl(this, ID_MPAUSEATTIME, wxT("0"),
        wxPoint(160,339), wxSize(80,19), 0, wxDefaultValidator,
        wxT("mPauseAfterTime"));
	mPauseAfterTime->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mPauseAtTime = new wxTextCtrl(this, ID_MPAUSEATTIME, wxT("0"),
        wxPoint(160,312), wxSize(80,19), 0, wxDefaultValidator,
        wxT("mPauseAtTime"));
	mPauseAtTime->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText10 = new wxStaticText(this, ID_WXSTATICTEXT10,
        wxT("Synchronization:"), wxPoint(12,421), wxDefaultSize, 0,
        wxT("WxStaticText10"));
	WxStaticText10->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	wxArrayString arrayStringFor_mDisplayTime;
	arrayStringFor_mDisplayTime.Add(wxT("Seconds"));
	arrayStringFor_mDisplayTime.Add(wxT("Minutes"));
	arrayStringFor_mDisplayTime.Add(wxT("Hours"));
	arrayStringFor_mDisplayTime.Add(wxT("Days"));
	arrayStringFor_mDisplayTime.Add(wxT("Weeks"));
	arrayStringFor_mDisplayTime.Add(wxT("Months"));
	arrayStringFor_mDisplayTime.Add(wxT("Years"));
	mDisplayTime = new wxComboBox(this, ID_MDISPLAYTIME, wxT(""),
        wxPoint(205,239),wxSize(90,21), arrayStringFor_mDisplayTime,
        wxCB_READONLY, wxDefaultValidator, wxT("mDisplayTime"));
	mDisplayTime->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	wxArrayString arrayStringFor_mModelUnits;
	arrayStringFor_mModelUnits.Add(wxT("Seconds"));
	arrayStringFor_mModelUnits.Add(wxT("Minutes"));
	arrayStringFor_mModelUnits.Add(wxT("Hours"));
	arrayStringFor_mModelUnits.Add(wxT("Days"));
	arrayStringFor_mModelUnits.Add(wxT("Weeks"));
	arrayStringFor_mModelUnits.Add(wxT("Months"));
	arrayStringFor_mModelUnits.Add(wxT("Years"));
	mModelUnits = new wxComboBox(this, ID_MMODELUNITS, wxT(""),
        wxPoint(205,202), wxSize(90,21), arrayStringFor_mModelUnits,
        wxCB_READONLY, wxDefaultValidator, wxT("mModelUnits"));
	mModelUnits->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText9 = new wxStaticText(this, ID_WXSTATICTEXT9,
        wxT("to the units used in your models:"), wxPoint(15,213),
        wxDefaultSize, 0, wxT("WxStaticText9"));
	WxStaticText9->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText8 = new wxStaticText(this, ID_WXSTATICTEXT8,
        wxT("Select the time units that correspond"), wxPoint(15,198),
        wxDefaultSize, 0, wxT("WxStaticText8"));
	WxStaticText8->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText7 = new wxStaticText(this, ID_WXSTATICTEXT7, wxT("Hours"),
        wxPoint(235,134), wxDefaultSize, 0, wxT("WxStaticText7"));
	WxStaticText7->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText6 = new wxStaticText(this, ID_WXSTATICTEXT6, wxT("Seconds"),
        wxPoint(235,109), wxDefaultSize, 0, wxT("WxStaticText6"));
	WxStaticText6->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText5 = new wxStaticText(this, ID_WXSTATICTEXT5, wxT("Hours"),
        wxPoint(235,85), wxDefaultSize, 0, wxT("WxStaticText5"));
	WxStaticText5->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mTimeNow = new wxTextCtrl(this, ID_MTIMENOW, wxT("0"), wxPoint(109,138),
        wxSize(121,19), 0, wxDefaultValidator, wxT("mTimeNow"));
	mTimeNow->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mDis = new wxTextCtrl(this, ID_MDIS, wxT("2"), wxPoint(109,112),
        wxSize(121,19), 0, wxDefaultValidator, wxT("mDis"));
	mDis->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mCom = new wxTextCtrl(this, ID_MCOM, wxT("0.01"), wxPoint(109,87),
        wxSize(121,19), 0, wxDefaultValidator, wxT("mCom"));
	mCom->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText4 = new wxStaticText(this, ID_WXSTATICTEXT4, wxT("Time now:"),
        wxPoint(15,140), wxDefaultSize, 0, wxT("WxStaticText4"));
	WxStaticText4->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText3 = new wxStaticText(this, ID_WXSTATICTEXT3,
        wxT("Display update:"), wxPoint(15,114), wxDefaultSize, 0,
        wxT("WxStaticText3"));
	WxStaticText3->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText2 = new wxStaticText(this, ID_WXSTATICTEXT2,
        wxT("Communication:"), wxPoint(15,87), wxDefaultSize, 0,
        wxT("WxStaticText2"));
	WxStaticText2->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticText1 = new wxStaticText(this, ID_WXSTATICTEXT1,
        wxT("Change simulation run mode:"), wxPoint(12,30), wxDefaultSize,
        0, wxT("WxStaticText1"));
	WxStaticText1->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	cancelButton = new wxButton(this, ID_CANCELBUTTON, wxT("Cancel"),
        wxPoint(167,456), wxSize(75,25), 0, wxDefaultValidator,
        wxT("cancelButton"));
	cancelButton->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	okButton = new wxButton(this, ID_OKBUTTON, wxT("OK"), wxPoint(78,456),
        wxSize(75,25), 0, wxDefaultValidator, wxT("okButton"));
	okButton->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticBox4 = new wxStaticBox(this, ID_WXSTATICBOX4,
        wxT("Simulation Control"), wxPoint(5,282), wxSize(297,163));
	WxStaticBox4->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	WxStaticBox3 = new wxStaticBox(this, ID_WXSTATICBOX3, wxT("Time Units"),
        wxPoint(5,174), wxSize(297,103));
	WxStaticBox3->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	Time_Control = new wxStaticBox(this, ID_TIME_CONTROL, wxT("Time Control"),
        wxPoint(5,62), wxSize(297,107));
	Time_Control->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));

	mRunMode = new wxStaticBox(this, ID_LABEL1, wxT("Run Mode"), wxPoint(5,7),
        wxSize(297,50));
	mRunMode->SetFont(wxFont(8, wxSWISS, wxNORMAL,wxNORMAL, false,
        wxT("Tahoma")));
	
	mMode->SetValue("Steady State");
	SetMode();
	mModelUnits->SetValue("Hours");
	mDisplayTime->SetValue("Hours");
	mSynchro->SetValue("Full");
}
////////////////////////////////////////////////////////////////////////////////
void ADRunDialog::OnClose(wxCloseEvent& /*event*/)
{
	Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void ADRunDialog::okButtonClick(wxCommandEvent& event)
{
}
////////////////////////////////////////////////////////////////////////////////
void ADRunDialog::cancelButtonClick(wxCommandEvent& event)
{
}
////////////////////////////////////////////////////////////////////////////////
void ADRunDialog::mPauseAfterClick(wxCommandEvent& event)
{
}
////////////////////////////////////////////////////////////////////////////////
void ADRunDialog::mModeSelected(wxCommandEvent& event )
{
    SetMode();
}
////////////////////////////////////////////////////////////////////////////////
void ADRunDialog::SetMode()
{
    if( mMode->GetValue().CompareTo("Steady State") == 0  )
    {
        mPauseAt->Enable(false);
        mRTS->Enable(false);
        mPauseAfter->Enable(false);
        WxStaticText15->Enable(false);
        WxStaticText14->Enable(false);
        WxStaticText13->Enable(false);
        WxStaticText12->Enable(false);
        WxStaticText11->Enable(false);
        mRTSFactor->Enable(false);
        mPauseAfterTime->Enable(false);
        mPauseAtTime->Enable(false);
        WxStaticText10->Enable(true);
        mDisplayTime->Enable(false);
        mModelUnits->Enable(false);
        WxStaticText9->Enable(false);
        WxStaticText8->Enable(false);
        WxStaticText7->Enable(false);
        WxStaticText6->Enable(false);
        WxStaticText5->Enable(false);
        mTimeNow->Enable(false);
        mDis->Enable(false);
        mCom->Enable(false);
        WxStaticText4->Enable(false);
        WxStaticText3->Enable(false);
        WxStaticText2->Enable(false);
        mSynchro->Enable(true);
    }
    else if( mMode->GetValue().CompareTo("Optimization") == 0  )
    {
        mPauseAt->Enable(false);
        mRTS->Enable(false);
        mPauseAfter->Enable(false);
        WxStaticText15->Enable(false);
        WxStaticText14->Enable(false);
        WxStaticText13->Enable(false);
        WxStaticText12->Enable(false);
        WxStaticText11->Enable(false);
        mRTSFactor->Enable(false);
        mPauseAfterTime->Enable(false);
        mPauseAtTime->Enable(false);
        WxStaticText10->Enable(false);
        mDisplayTime->Enable(false);
        mModelUnits->Enable(false);
        WxStaticText9->Enable(false);
        WxStaticText8->Enable(false);
        WxStaticText7->Enable(false);
        WxStaticText6->Enable(false);
        WxStaticText5->Enable(false);
        mTimeNow->Enable(false);
        mDis->Enable(false);
        mCom->Enable(false);
        WxStaticText4->Enable(false);
        WxStaticText3->Enable(false);
        WxStaticText2->Enable(false);
        mSynchro->Enable(false);
    }
    else if( mMode->GetValue().CompareTo("Estimation") == 0 )
    {
        mPauseAt->Enable(false);
        mRTS->Enable(false);
        mPauseAfter->Enable(false);
        WxStaticText15->Enable(false);
        WxStaticText14->Enable(false);
        WxStaticText13->Enable(false);
        WxStaticText12->Enable(false);
        WxStaticText11->Enable(false);
        mRTSFactor->Enable(false);
        mPauseAfterTime->Enable(false);
        mPauseAtTime->Enable(false);
        WxStaticText10->Enable(false);
        mDisplayTime->Enable(false);
        mModelUnits->Enable(false);
        WxStaticText9->Enable(false);
        WxStaticText8->Enable(false);
        WxStaticText7->Enable(false);
        WxStaticText6->Enable(false);
        WxStaticText5->Enable(false);
        mTimeNow->Enable(false);
        mDis->Enable(false);
        mCom->Enable(false);
        WxStaticText4->Enable(false);
        WxStaticText3->Enable(false);
        WxStaticText2->Enable(false);
        mSynchro->Enable(false);
    }
    else if( mMode->GetValue().CompareTo("Dynamic") == 0 )
    {
        mPauseAt->Enable(true);
        mRTS->Enable(true);
        mPauseAfter->Enable(true);
        WxStaticText15->Enable(true);
        WxStaticText14->Enable(true);
        WxStaticText13->Enable(true);
        WxStaticText12->Enable(true);
        WxStaticText11->Enable(true);
        mRTSFactor->Enable(true);
        mPauseAfterTime->Enable(true);
        mPauseAtTime->Enable(true);
        WxStaticText10->Enable(true);
        mDisplayTime->Enable(true);
        mModelUnits->Enable(true);
        WxStaticText9->Enable(true);
        WxStaticText8->Enable(true);
        WxStaticText7->Enable(true);
        WxStaticText6->Enable(true);
        WxStaticText5->Enable(true);
        mTimeNow->Enable(true);
        mDis->Enable(true);
        mCom->Enable(true);
        WxStaticText4->Enable(true);
        WxStaticText3->Enable(true);
        WxStaticText2->Enable(true);
        mSynchro->Enable(true);
    }
    else if( mMode->GetValue().CompareTo("Initialization") == 0 )
    {
        mPauseAt->Enable(false);
        mRTS->Enable(false);
        mPauseAfter->Enable(false);
        WxStaticText15->Enable(true);
        WxStaticText14->Enable(true);
        WxStaticText13->Enable(false);
        WxStaticText12->Enable(false);
        WxStaticText11->Enable(false);
        mRTSFactor->Enable(false);
        mPauseAfterTime->Enable(false);
        mPauseAtTime->Enable(false);
        WxStaticText10->Enable(true);
        mDisplayTime->Enable(true);
        mModelUnits->Enable(true);
        WxStaticText9->Enable(true);
        WxStaticText8->Enable(true);
        WxStaticText7->Enable(false);
        WxStaticText6->Enable(false);
        WxStaticText5->Enable(false);
        mTimeNow->Enable(false);
        mDis->Enable(false);
        mCom->Enable(false);
        WxStaticText4->Enable(false);
        WxStaticText3->Enable(false);
        WxStaticText2->Enable(false);
        mSynchro->Enable(true);
    }
}
