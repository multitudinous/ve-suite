///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Apr 16 2008)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#include "DynamicVehicleSimToolBase.h"

///////////////////////////////////////////////////////////////////////////
using namespace dvst;

DynamicVehicleSimToolBase::DynamicVehicleSimToolBase( wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style ) 
    :
    UIDialog( parent, id, title )
{
	this->SetSizeHints( wxDefaultSize, wxDefaultSize );
	
	wxBoxSizer* bSizer1;
	bSizer1 = new wxBoxSizer( wxVERTICAL );
	
	wxStaticBoxSizer* sbSizer1;
	sbSizer1 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Simulator Computer Info") ), wxVERTICAL );
	
	wxBoxSizer* bSizer8;
	bSizer8 = new wxBoxSizer( wxHORIZONTAL );
	
	wxStaticBoxSizer* sbSizer4;
	sbSizer4 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Computer Name") ), wxVERTICAL );
	
	m_textCtrl1 = new wxTextCtrl( this, wxID_ANY, wxT("225.0.0.37"), wxDefaultPosition, wxDefaultSize, 0 );
	sbSizer4->Add( m_textCtrl1, 0, wxEXPAND, 5 );
	
	bSizer8->Add( sbSizer4, 1, wxLEFT|wxRIGHT, 5 );
	
	wxStaticBoxSizer* sbSizer5;
	sbSizer5 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Port Number") ), wxVERTICAL );
	
	m_textCtrl2 = new wxTextCtrl( this, wxID_ANY, wxT("12345"), wxDefaultPosition, wxDefaultSize, 0 );
	sbSizer5->Add( m_textCtrl2, 0, wxEXPAND, 5 );
	
	bSizer8->Add( sbSizer5, 1, wxLEFT|wxRIGHT, 5 );
	
	sbSizer1->Add( bSizer8, 0, wxEXPAND, 5 );
	
	bSizer1->Add( sbSizer1, 0, wxALL|wxEXPAND, 5 );
	
	wxStaticBoxSizer* sbSizer3;
	sbSizer3 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Simulator Controls") ), wxHORIZONTAL );
	
	m_toggleBtn1 = new wxToggleButton( this, wxID_ANY, wxT("Start/Stop"), wxDefaultPosition, wxDefaultSize, 0 );
	sbSizer3->Add( m_toggleBtn1, 0, wxRIGHT, 5 );
	
	m_resetButton = new wxButton( this, wxID_ANY, wxT("Reset"), wxDefaultPosition, wxDefaultSize, 0 );
	sbSizer3->Add( m_resetButton, 0, wxALIGN_CENTER_HORIZONTAL|wxLEFT, 5 );
	
	bSizer1->Add( sbSizer3, 0, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5 );
	
	wxStaticBoxSizer* sbSizer2;
	sbSizer2 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Geometry Data Mapping") ), wxVERTICAL );
	
	wxBoxSizer* bSizer2;
	bSizer2 = new wxBoxSizer( wxVERTICAL );
	
	m_scrolledWindow1 = new wxScrolledWindow( this, wxID_ANY, wxDefaultPosition, wxSize( -1,100 ), wxHSCROLL|wxSUNKEN_BORDER|wxVSCROLL );
	m_scrolledWindow1->SetScrollRate( 5, 5 );
	m_scrolledWindow1->SetMinSize( wxSize( -1,100 ) );
	
	wxBoxSizer* bSizer9;
	bSizer9 = new wxBoxSizer( wxVERTICAL );
	
	bSizer9->SetMinSize( wxSize( -1,100 ) ); 
	wxBoxSizer* bSizer4;
	bSizer4 = new wxBoxSizer( wxHORIZONTAL );
	
	m_staticText1 = new wxStaticText( m_scrolledWindow1, wxID_ANY, wxT("1"), wxDefaultPosition, wxDefaultSize, 0 );
	m_staticText1->Wrap( -1 );
	bSizer4->Add( m_staticText1, 0, wxALIGN_CENTER, 5 );
	
	wxArrayString m_choice1Choices;
	m_choice1 = new wxChoice( m_scrolledWindow1, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice1Choices, 0 );
	m_choice1->SetSelection( 0 );
	bSizer4->Add( m_choice1, 0, wxALIGN_CENTER, 5 );
	
	bSizer9->Add( bSizer4, 0, 0, 5 );
	
	wxBoxSizer* bSizer41;
	bSizer41 = new wxBoxSizer( wxHORIZONTAL );
	
	m_staticText11 = new wxStaticText( m_scrolledWindow1, wxID_ANY, wxT("2"), wxDefaultPosition, wxDefaultSize, 0 );
	m_staticText11->Wrap( -1 );
	bSizer41->Add( m_staticText11, 0, wxALIGN_CENTER, 5 );
	
	wxArrayString m_choice11Choices;
	m_choice11 = new wxChoice( m_scrolledWindow1, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice11Choices, 0 );
	m_choice11->SetSelection( 0 );
	bSizer41->Add( m_choice11, 0, wxALIGN_CENTER, 5 );
	
	bSizer9->Add( bSizer41, 0, 0, 5 );
	
	m_scrolledWindow1->SetSizer( bSizer9 );
	m_scrolledWindow1->Layout();
	bSizer2->Add( m_scrolledWindow1, 1, wxEXPAND | wxALL, 5 );
	
	wxBoxSizer* bSizer3;
	bSizer3 = new wxBoxSizer( wxHORIZONTAL );
	
	m_button4 = new wxButton( this, wxID_ANY, wxT("Add"), wxDefaultPosition, wxDefaultSize, 0 );
	bSizer3->Add( m_button4, 0, wxALL, 5 );
	
	m_button5 = new wxButton( this, wxID_ANY, wxT("Remove"), wxDefaultPosition, wxDefaultSize, 0 );
	bSizer3->Add( m_button5, 0, wxALL, 5 );
	
	bSizer2->Add( bSizer3, 0, wxALIGN_CENTER_HORIZONTAL, 5 );
	
	wxStaticBoxSizer* sbSizer6;
	sbSizer6 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("User Constrained Geometry") ), wxVERTICAL );
	
	wxArrayString m_choice3Choices;
	m_choice3 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice3Choices, 0 );
	m_choice3->SetSelection( 0 );
	sbSizer6->Add( m_choice3, 0, 0, 5 );
	
	bSizer2->Add( sbSizer6, 0, wxBOTTOM|wxEXPAND|wxTOP, 5 );
	
	sbSizer2->Add( bSizer2, 0, wxEXPAND, 5 );
	
	bSizer1->Add( sbSizer2, 0, wxALL|wxEXPAND, 5 );
	
	m_sdbSizer1 = new wxStdDialogButtonSizer();
	m_sdbSizer1OK = new wxButton( this, wxID_OK );
	m_sdbSizer1->AddButton( m_sdbSizer1OK );
	m_sdbSizer1Apply = new wxButton( this, wxID_APPLY );
	m_sdbSizer1->AddButton( m_sdbSizer1Apply );
	m_sdbSizer1->Realize();
	bSizer1->Add( m_sdbSizer1, 0, wxALIGN_CENTER, 5 );
	
	this->SetSizer( bSizer1 );
	this->Layout();
	
	// Connect Events
	m_textCtrl1->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnComputerNameEnter ), NULL, this );
	m_textCtrl2->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnPortNumberEnter ), NULL, this );
	m_toggleBtn1->Connect( wxEVT_COMMAND_TOGGLEBUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnStartStopButton ), NULL, this );
	m_resetButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnResetSimulation ), NULL, this );
	m_choice1->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
	m_choice11->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
	m_button4->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnAddGeometryGroupButton ), NULL, this );
	m_button5->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRemoveGeometryGroupButton ), NULL, this );
	m_choice3->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnConstrainedGeometrySelection ), NULL, this );
	m_sdbSizer1Apply->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
	m_sdbSizer1OK->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnOKButton ), NULL, this );
}

DynamicVehicleSimToolBase::~DynamicVehicleSimToolBase()
{
	// Disconnect Events
	m_textCtrl1->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnComputerNameEnter ), NULL, this );
	m_textCtrl2->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnPortNumberEnter ), NULL, this );
	m_toggleBtn1->Disconnect( wxEVT_COMMAND_TOGGLEBUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnStartStopButton ), NULL, this );
	m_resetButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnResetSimulation ), NULL, this );
	m_choice1->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
	m_choice11->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
	m_button4->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnAddGeometryGroupButton ), NULL, this );
	m_button5->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRemoveGeometryGroupButton ), NULL, this );
	m_choice3->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnConstrainedGeometrySelection ), NULL, this );
	m_sdbSizer1Apply->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
	m_sdbSizer1OK->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnOKButton ), NULL, this );
}
